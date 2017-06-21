{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, QuasiQuotes
    , DataKinds, ScopedTypeVariables, OverloadedStrings #-}
module Language.ANTLR4.G4 (g4) where

import Text.ANTLR.Allstar.Grammar
import Text.ANTLR.Parser
import qualified Text.ANTLR.LR as LR
--import Language.Chisel.Tokenizer
import qualified Text.ANTLR.Lex.Tokenizer as T
import qualified Text.ANTLR.Set as S
import Text.ANTLR.Set (Hashable(..), Generic(..))
import Text.ANTLR.Pretty
import Control.Arrow ( (&&&) )
import Text.ANTLR.Lex.Regex

import Language.Haskell.TH.Quote (QuasiQuoter(..))
import qualified Language.Haskell.TH as TH
import Language.ANTLR4
import Language.ANTLR4.Syntax

import Debug.Trace as D

[antlr4|
  grammar G4;

  decls : decl1 ';'                 -> list
        | decl1 ';' decls           -> cons
        ;

  decl1 : 'grammar' UpperID         -> Grammar
        | LowerID ':' prods         -> Prod
        | UpperID ':' lexemeRHS     -> lexDecl
        ;

  prods : prodRHS                   -> list
        | prodRHS '|' prods         -> cons
        ;

  lexemeRHS : regex '->' directive  -> lexemeDirective
            | regex                 -> lexemeNoDir
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

  alpha : Literal                   -> GTerm
        | LowerID                   -> GTerm
        | UpperID                   -> GNonTerm
        ;

  UpperID : [A-Z][a-zA-Z0-9_]*      -> String;
  LowerID : [a-z][a-zA-Z0-9_]*      -> String;
  Literal     : '\'' (~ '\'')+ '\'' -> stripQuotes;
  LineComment : '//' (~ '\n')* '\n' -> String;
  WS          : [ \t\n\r\f\v]+      -> String;
  
  SetChar     : ~ '\]' ;
  EscapedChar : '\\\\' [tnrfv]      -> readEscape ;


  // Regex Stuff:

  regex   : regex1 '?'
          | regex1 '*'
          | regex1 '+'
          | regex1
          ;

  regex1  : '[' charSet ']'           -> CharSet
          | Literal                   -> Literal
          | UpperID                   -> Named
          | '(' regex ')'
          | unionR                    -> Concat
          | regex regex
          ;

  unionR  : regex '|' regex         -> list2
          | regex '|' unionR        -> cons
          ;

  charSet : charSet1                -> list
          | charSet1 charSet        -> cons
          ;

  charSet1 : SetChar '-' SetChar    -> range
           | SetChar
           | EscapedChar
           ;

  // LiteralR : '\'' (~ '\'')+ '\''    -> stripQuotes ;

|]

isWhitespace (T.Token T_LineComment _) = True
isWhitespace (T.Token T_WS _) = True
isWhitespace _ = False

g4_codeGen :: String -> TH.Q [TH.Dec]
g4_codeGen input = do
  loc <- TH.location
  let fileName = TH.loc_filename loc
  let (line,column) = TH.loc_start loc

  case (glrParse . filter (not . isWhitespace)) $ tokenize input of
    LR.ResultAccept ast -> codeGen ast
    err                 -> error $ show err

-- TODO: Convert a Universal AST into a [G4S.G4]
codeGen ast = trace (show ast) (return [])

g4 :: QuasiQuoter
g4 = QuasiQuoter
  (error "parse exp")
  (error "parse pattern")
  (error "parse type")
  g4_codeGen

