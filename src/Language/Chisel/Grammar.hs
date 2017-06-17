{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, QuasiQuotes
    , DataKinds, ScopedTypeVariables, OverloadedStrings #-}
module Language.Chisel.Grammar
  ( parse, tokenize, ChiselNTSymbol(..), ChiselTSymbol(..), ChiselAST
  , lowerID, upperID, prim, int, arrow, lparen, rparen, pound
  , vertbar, colon, comma, atsymbol, carrot, dot, linecomm, ws
  , Primitive(..), chisel, TokenValue(..)
  ) where

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

data Primitive = Page | Word | Byte | Bit
  deriving (Show, Eq, Ord, Generic, Hashable)

lexeme2prim "page"  = Just Page
lexeme2prim "pages" = Just Page
lexeme2prim "word"  = Just Word
lexeme2prim "words" = Just Word
lexeme2prim "byte"  = Just Byte
lexeme2prim "bytes" = Just Byte
lexeme2prim "bit"   = Just Bit
lexeme2prim "bits"  = Just Bit
lexeme2prim _       = Nothing

instance Read Primitive where
  readsPrec _ input = case lexeme2prim input of
    Just x  -> [(x,"")]
    Nothing -> []

instance Prettify Primitive where prettify = rshow

[antlr4|
  grammar Chisel;
  chiselProd : prodSimple
             | '(' prodSimple ')'
             ;

  prodSimple : prodID formals magnitude alignment '->' group
             | prodID formals '->' group
             | prodID magnitude alignment '->' group
             | prodID magnitude '->' group
             | LowerID prodID magnitude alignment '->' group
             ;

  formals : LowerID formals
          | LowerID
          ;

  magnitude : '|' '#' sizeArith '|'
            | '|'     sizeArith '|'
            | '|'     prodID    '|'
            ;

  alignment : '@' '(' sizeArith ')';

  group :     tupleExp1
        | '(' tupleExp  ')'
        ;
  
  tupleExp : tupleExp1
           | tupleExp1 ',' tupleExp
           ;

  tupleExp1 : prodID
            | '#' chiselProd
            | '#' sizeArith
            | '(' flags ')'
            | chiselProd
            | sizeArith
            | label
            | arith chiselProd
            | arith prodApp
            | sizeArith
            | '(' labels ')'
            ;

  flags : prodID
        | prodID '|' flags
        ;

  labels : label
         | label '|' labels
         ;

  label : LowerID ':' labelExp
        ;

  labelExp : '#' chiselProd
           | '#' prodApp
           | '#' sizeArith
           | chiselProd
           | prodApp
           | sizeArith
           ;

  prodApp : prodID prodApp
          | prodID
          ;

  sizeArith : arith Prim
            | Prim
            ;
  
  arith : INT
        | LowerID
        | INT '^' LowerID
        | sizeArith Prim
        ;

  prodID  : UpperID
          | UpperID '.' prodID
          ;

  carret : '^';

  Prim     : ( 'bit' | 'byte' ) 's'?      -> Primitive;
  ArchPrim : ( 'page' | 'word' ) 's'?     -> Primitive;
  LowerID  : [a-z][a-zA-Z0-9_]*           -> String;
  UpperID  : [A-Z][a-zA-Z0-9_]*           -> String;
  INT      : [0-9]+                       -> Int;
  LineComment : '//' (~ '\n')* '\n'       -> String;
  WS      : [ \t\n\r\f\v]+                -> String;
|]

-- Types used to the right of the '->' directive must instance Read

isWhitespace (T.Token T_LineComment _) = True
isWhitespace (T.Token T_WS _) = True
isWhitespace _ = False

{- Helper functions to construct all the various Tokens from either the desired
 - (arbitrary) lexeme or by looking it up based on the static lexeme it always
 - matches. -}
lowerID  x = T.Token T_LowerID $   V_LowerID x
upperID  x = T.Token T_UpperID $   V_UpperID x
prim     x = T.Token T_Prim    $   V_Prim x
int      x = T.Token T_INT     $   V_INT x
arrow      = lookupToken "->"
lparen     = lookupToken "("
rparen     = lookupToken ")"
pound      = lookupToken "#"
vertbar    = lookupToken "|"
colon      = lookupToken ":"
comma      = lookupToken ","
atsymbol   = lookupToken "@"
carrot     = lookupToken "^"
dot        = lookupToken "."
linecomm x = T.Token T_LineComment $ V_LineComment x
ws       x = T.Token T_WS          $ V_WS x

parse = glrParse . filter (not . isWhitespace)

