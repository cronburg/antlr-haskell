{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, QuasiQuotes
    , DataKinds, ScopedTypeVariables, OverloadedStrings #-}
module Language.Chisel.Grammar
  ( parse, tokenize, ChiselNTSymbol(..), ChiselTSymbol(..), ChiselAST
  , lowerID, upperID, prim, int, arrow, lparen, rparen, pound
  , vertbar, colon, comma, atsymbol, carrot, dot, linecomm, ws
  , Primitive(..), chiselGrammar, TokenValue(..)
  ) where

import Text.ANTLR.Grammar
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
import Language.ANTLR4.G4
import Language.Chisel.Syntax

import Debug.Trace as D

[g4|
  grammar Chisel;
  chiselProd : prodSimple
             | '(' prodSimple ')'
             ;

  prodSimple : prodID formals magnitude alignment '->' group    -> prodFMA
             | prodID formals '->' group                        -> prodF
             | prodID magnitude alignment '->' group            -> prodMA
             | prodID magnitude '->' group                      -> prodM
             | LowerID prodID magnitude alignment '->' group    -> prodNMA
             ;

  formals : LowerID formals             -> cons
          | LowerID                     -> list
          ;

  magnitude : '|' '#' sizeArith '|'     -> magWild
            | '|'     sizeArith '|'     -> magNorm
            | '|'     prodID    '|'     -> magID
            ;

  alignment : '@' '(' sizeArith ')';

  group :     groupExp1                 -> list
        | '(' groupExp  ')'
        ;
  
  groupExp : groupExp1                  -> list
           | groupExp1 ',' groupExp     -> cons
           ;

  groupExp1 : '#' chiselProd            -> gProdWild
            | '#' sizeArith             -> gSizeWild
            | '(' flags ')'             -> GFlags
            | chiselProd                -> gProdNorm
            | sizeArith                 -> gSizeNorm
            | label                     -> GLabel
            | arith chiselProd          -> gProdArith
            | arith prodApp             -> GProdApp
            | '(' labels ')'            -> GLabels
            ;

  flags : prodID                        -> list
        | prodID '|' flags              -> cons
        ;

  labels : label                        -> list
         | label '|' labels             -> cons
         ;

  label : LowerID ':' labelExp    -> Label
        ;

  labelExp : '#' chiselProd       -> lProdWild
           | '#' prodApp          -> lProdAppWild
           | '#' sizeArith        -> lSizeWild
           | chiselProd           -> lProd
           | prodApp              -> lProdApp
           | sizeArith            -> lSize
           ;

  prodApp : prodID prodApp        -> cons
          | prodID                -> list
          ;

  sizeArith : arith Prim          -> SizeArith
            | Prim                -> singleArith
            ;
  
  arith : INT                     -> SizeInt
        | LowerID                 -> SizeID
        | arith '^' arith         -> SizeExp
        ;

  prodID  : UpperID               -> id
          | UpperID '.' prodID    -> append
          ;

  Prim     : ( 'bit' | 'byte' ) 's'?      -> Primitive;
  ArchPrim : ( 'page' | 'word' ) 's'?     -> Primitive;
  UpperID  : [A-Z][a-zA-Z0-9_]*           -> String;
  LowerID  : [a-z][a-zA-Z0-9_]*           -> String;
  INT      : [0-9]+                       -> Int;
  LineComment : '//' (~ '\n')* '\n'       -> String;
  WS      : [ \t\n\r\f\v]+                -> String;
|]

-- Types used to the right of the '->' directive must instance Read

isWhitespace T_LineComment = True
isWhitespace T_WS = True
isWhitespace _ = False

{- Helper functions to construct all the various Tokens from either the desired
 - (arbitrary) lexeme or by looking it up based on the static lexeme it always
 - matches. -}
lowerID  x = T.Token T_LowerID (V_LowerID x) (length x)
upperID  x = T.Token T_UpperID (V_UpperID x) (length x)
prim     x = T.Token T_Prim    (V_Prim x)    (length $ show x)
int      x = T.Token T_INT     (V_INT x)     (length $ show x)
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
linecomm x = T.Token T_LineComment (V_LineComment x) (length x)
ws       x = T.Token T_WS          (V_WS x)          (length x)

parse = glrParse isWhitespace

