{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, QuasiQuotes
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances #-}
module Language.ANTLR4.G4 (g4) where

import Text.ANTLR.Grammar
import Text.ANTLR.Parser
import qualified Text.ANTLR.LR as LR
--import Language.Chisel.Tokenizer
import qualified Text.ANTLR.Lex.Tokenizer as T
import qualified Text.ANTLR.Set as S
import Text.ANTLR.Set (Hashable(..), Generic(..))
import Text.ANTLR.Pretty
import Control.Arrow ( (&&&) )
import Text.ANTLR.Lex.Regex (regex2dfa)

import Language.Haskell.TH.Quote (QuasiQuoter(..))
import qualified Language.Haskell.TH as TH
import Language.ANTLR4.Boot.Quote (antlr4)
import Language.ANTLR4.Syntax
import qualified Language.ANTLR4.Boot.Syntax  as G4S
import qualified Language.ANTLR4.Boot.Quote   as G4Q

import Debug.Trace as D

[antlr4|
  grammar G4;

  decls : decl1 ';'                 -> list
        | decl1 ';' decls           -> cons
        ;

  decl1 : 'grammar' UpperID         -> G4S.Grammar
        | LowerID ':' prods         -> G4S.Prod
        | UpperID ':' lexemeRHS     -> lexDecl
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

  directive : UpperID
            | LowerID
            ;

  alphas : alpha                    -> list
         | alpha alphas             -> cons
         ;

  alpha : Literal                   -> G4S.GTerm
        | LowerID                   -> G4S.GNonTerm
        | UpperID                   -> G4S.GNonTerm
        ;

  UpperID : [A-Z][a-zA-Z0-9_]*      -> String;
  LowerID : [a-z][a-zA-Z0-9_]*      -> String;
  Literal     : '\'' (~ '\'')+ '\'' -> stripQuotesReadEscape;
  LineComment : '//' (~ '\n')* '\n' -> String;
  
  EscapedChar : '\\\\' [tnrfv]      -> readEscape ;
  SetChar     : ~ '\]'              -> char ;
  WS          : [ \t\n\r\f\v]+      -> String;

  // Regex Stuff:

  regexes1 : regexes                -> G4S.Concat
           ;

  regexes : regex                   -> list
          | regex regexes           -> cons
          ;

  regex   :     regex1 '?'          -> G4S.Question
          |     regex1 '*'          -> G4S.Kleene
          |     regex1 '+'          -> G4S.PosClos
          | '~' regex1              -> G4S.Negation
          |     regex1              -> id
          ;

  regex1  : '[' charSet ']'           -> G4S.CharSet
          | Literal                   -> literalRegex
          | UpperID                   -> G4S.Named
          | '(' regex ')'
          | unionR                    -> G4S.Union
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

  // LiteralR : '\'' (~ '\'')+ '\''    -> stripQuotes ;

|]

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
        else error $ pshow' s
    err                 -> error $ pshow' err

-- TODO: Convert a Universal AST into a [G4S.G4]
codeGen (LR.ResultAccept ast) = G4Q.g4_decls $ ast2decls ast --trace (show ast) (return [])

g4 :: QuasiQuoter
g4 = QuasiQuoter
  (error "parse exp")
  (error "parse pattern")
  (error "parse type")
  g4_codeGen

