{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, QuasiQuotes
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, DeriveDataTypeable #-}
module Language.ANTLR4.G4 (g4) where

import Control.Arrow ( (&&&) )
import Data.Char (isUpper)

import Text.ANTLR.Grammar
import Text.ANTLR.Parser
import qualified Text.ANTLR.LR as LR
--import Language.Chisel.Tokenizer
import Text.ANTLR.Lex.Tokenizer as T
import qualified Text.ANTLR.Set as S
import Text.ANTLR.Set (Hashable(..), Generic(..))
import Text.ANTLR.Pretty
import Text.ANTLR.Lex.Regex (regex2dfa)
import Data.Data (Data(..))
import Language.Haskell.TH.Lift (Lift(..))

import Language.Haskell.TH.Quote (QuasiQuoter(..))
import qualified Language.Haskell.TH as TH
import Language.ANTLR4.Boot.Quote (antlr4)
import Language.ANTLR4.Syntax
import qualified Language.ANTLR4.Boot.Syntax  as G4S
--import Language.ANTLR4.Boot.Syntax (Regex(..), G4(..))
import qualified Language.ANTLR4.Boot.Quote   as G4Q

import Debug.Trace as D

gterm         = G4S.GTerm    G4S.NoAnnot
gnonTerm      = G4S.GNonTerm G4S.NoAnnot

maybeGTerm    = G4S.GTerm    (G4S.Regular '?')
maybeGNonTerm = G4S.GNonTerm (G4S.Regular '?')

starGTerm    = G4S.GTerm    (G4S.Regular '*')
starGNonTerm = G4S.GNonTerm (G4S.Regular '*')

plusGTerm    = G4S.GTerm    (G4S.Regular '+')
plusGNonTerm = G4S.GNonTerm (G4S.Regular '+')

regexAnyChar = G4S.Negation (G4S.CharSet [])

-- TODO: Qualified names on RHS / as g4 directive
g4sGrammar = G4S.Grammar
g4sProd    = G4S.Prod
g4sCharSet = G4S.CharSet
g4sPosClos = G4S.PosClos
g4sQuestion  = G4S.Question
g4sKleene    = G4S.Kleene
g4sNegation  = G4S.Negation
g4sConcat    = G4S.Concat
g4sUnion     = G4S.Union
g4sNamed     = G4S.Named

dUpper   = G4S.UpperD
dLower   = G4S.LowerD
dHaskell = G4S.HaskellD

concatWith cs [] = []
concatWith cs [x] = x
concatWith cs (x:xs) = x ++ cs ++ concatWith cs xs

dQual [] = G4S.UpperD []
dQual xs = case last xs of
  [] -> G4S.UpperD $ concatWith "." xs
  (a:as)
    | isUpper a -> G4S.UpperD $ concatWith "." xs
    | otherwise -> G4S.LowerD $ concatWith "." xs

qDir l u = [l,u]

[antlr4|
  grammar G4;

  decls : decl1 ';'                 -> list
        | decl1 ';' decls           -> cons
        ;

  decl1 : 'grammar' UpperID                 -> g4sGrammar
        | LowerID ':' prods                 -> g4sProd
        | UpperID ':' lexemeRHS             -> lexDecl
        | 'fragment' UpperID ':' lexemeRHS  -> lexFragment
        ;

  prods : prodRHS                   -> list
        | prodRHS '|' prods         -> cons
        ;

  lexemeRHS : regexes1 '->' directive  -> lexemeDirective
            | regexes1                 -> lexemeNoDir
            ;

  prodRHS : alphas '->' directive   -> prodDirective
          | alphas                  -> prodNoDir
          ;

  directive : qDirective          -> dQual
            | UpperID             -> dUpper
            | LowerID             -> dLower
            | '${' HaskellExp '}' -> dHaskell
            ;

  qDirective  : UpperID '.' qDot -> qDir
              ;

  //qDirective : qDot                 -> list
  //           | qDot '.' qDirective  -> cons
  //           ;

  qDot  : UpperID
        | LowerID
        ;

  HaskellExp : ( ~ '}' )+ -> String;

  alphas : alpha                    -> list
         | alpha alphas             -> cons
				 | '(' alphas ')'

	// TODO:
				 | '(' alphas ')' '?'
				 | '(' alphas ')' '*'
				 | '(' alphas ')' '+'
         ;

  alpha : Literal '?'               -> maybeGTerm
        | LowerID '?'               -> maybeGNonTerm
        | UpperID '?'               -> maybeGNonTerm
        | Literal '*'               -> starGTerm
        | LowerID '*'               -> starGNonTerm
        | UpperID '*'               -> starGNonTerm
        | Literal '+'               -> plusGTerm
        | LowerID '+'               -> plusGNonTerm
        | UpperID '+'               -> plusGNonTerm
        | Literal                   -> gterm
        | LowerID                   -> gnonTerm
        | UpperID                   -> gnonTerm
        ;

  // Regex Stuff:

  regexes1 : regexes                -> g4sConcat
           ;

  regexes : regex                   -> list
          | regex regexes           -> cons
          ;

  regex   :     regex1 '?'          -> g4sQuestion
          |     regex1 '*'          -> g4sKleene
          |     regex1 '+'          -> g4sPosClos
          | '~' regex1              -> g4sNegation
          |     regex1              -> id
          ;

  regex1  : '[' charSet ']'           -> g4sCharSet
          | Literal                   -> literalRegex
          | UpperID                   -> g4sNamed
          | '(' regexes1 ')'
          | unionR                    -> g4sUnion
          | '.'                       -> regexAnyChar
          ;

  unionR  : regex '|' regex         -> list2
          | regex '|' unionR        -> cons
          ;

  charSet : charSet1                -> id
          | charSet1 charSet        -> append
          ;

  charSet1 : SetChar '-' SetChar    -> range
           | SetChar                -> list
           | EscapedChar            -> list
           ;

  UpperID : [A-Z][a-zA-Z0-9_]*      -> String;
  LowerID : [a-z][a-zA-Z0-9_]*      -> String;
  Literal     : '\'' (~ '\'')+ '\'' -> stripQuotesReadEscape;
  LineComment : '//' (~ '\n')* '\n' -> String;
  
  SetChar     : ~ ']'               -> char ;
  WS          : [ \t\n\r\f\v]+      -> String;
  EscapedChar : '\\' [tnrfv]      	-> readEscape ;

|]
-- LiteralR : '\'' (~ '\'')+ '\''    -> stripQuotes ;

isWhitespace T_LineComment = True
isWhitespace T_WS = True
isWhitespace _ = False

g4_codeGen :: String -> TH.Q [TH.Dec]
g4_codeGen input = do
  loc <- TH.location
  let fileName = TH.loc_filename loc
  let (line,column) = TH.loc_start loc

  --case (glrParse . filter (not . isWhitespace)) $ tokenize input of
  case glrParse isWhitespace input of
    r@(LR.ResultAccept ast) -> codeGen r
    LR.ResultSet    s   ->
      if S.size s == 1
        then codeGen (S.findMin s)
        else D.trace (pshow' s) $ codeGen (S.findMin s)
    err                 -> error $ pshow' err

-- TODO: Convert a Universal AST into a [G4S.G4]
codeGen (LR.ResultAccept ast) = G4Q.g4_decls $ ast2decls ast --trace (show ast) (return [])

g4 :: QuasiQuoter
g4 = QuasiQuoter
  (error "parse exp")
  (error "parse pattern")
  (error "parse type")
  g4_codeGen

