{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, QuasiQuotes
    , DataKinds, ScopedTypeVariables, OverloadedStrings #-}
module Language.ANTLR4.G4 where

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

import Language.ANTLR4

import Debug.Trace as D

[antlr4|
  grammar G4;

  decl : decl1 ';'
       | decl1 ';' decl
       ;

  decl1 : grammarD
        | prodD
        | lexemeD
        ;

  grammarD  : 'grammar' UpperID;
  prodD     : LowerID ':' prodRHS;
  lexemeD   : UpperID ':' lexemeRHS;

  lexemeRHS : regex '->' directive
            | regex
            ;

  prodRHS : alphas '->' directive
          | alphas
          ;

  alphas : alpha
         | alpha alphas
         ;

  alpha : LiteralTerm
        | LowerID     // Term (lexeme)
        | UpperID     // NonTerm
        ;

  UpperID : [A-Z][a-zA-Z0-9_]*      -> String;
  LowerID : [a-z][a-zA-Z0-9_]*      -> String;
  LiteralTerm : '\'' (~ '\'')* '\'' -> String;
  LineComment : '//' (~ '\n')* '\n' -> String;
  WS          : [ \t\n\r\f\v]+      -> String;

  // Regex Stuff:

  regex : charSet
        | LiteralR
        | UpperID
        | '(' regex ')'
        | regex '|' regex
        | regex regex
        ;
 
  charSet      : '[' charSetInner ']' ;
  
  charSetInner : charSetInner1 charSetInner
               | charSetInner1
               ;
  
  charSetInner1 : SetChar '-' SetChar
                | SetChar
                | EscapedChar
                ;

  SetChar     : ~ '\]'   ;
  EscapedChar : '\\\\' [tnrfv] ;

  LiteralR : '\'' (~ '\'')+ '\'' ;

|]

