{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, QuasiQuotes
    , DataKinds, ScopedTypeVariables #-}
module Language.Chisel.Parser
  ( parse, ChiselNTS(..), ChiselTS, ChiselAST
  ) where

import Text.ANTLR.Allstar.Grammar
import Text.ANTLR.Parser
import qualified Text.ANTLR.LR1 as P
--import Language.Chisel.Tokenizer
import qualified Text.ANTLR.Lex.Tokenizer as T
import qualified Text.ANTLR.Set as S
import Text.ANTLR.Set (Hashable(..), Generic(..))
import Text.ANTLR.Pretty

import Language.ANTLR4

import Debug.Trace as D

[antlr4|
  grammar Chisel;
  chiselProd : prodSimple
             | '(' prodSimple ')'
             ;

  prodSimple : UpperID formals magnitude alignment '->' group
             | UpperID formals '->' group
             ;

  formals : LowerID
          | LowerID formals
          ;

  magnitude : '|' '#' sizeArith '|'
            | '|'     sizeArith '|'
            ;

  alignment : '@' '(' sizeArith ')';

  group : tuple | alt | flags;

  tuple : 'TODO';
  alt   : 'TODO';
  flags : 'TODO';

  LowerID : [a-z][a-zA-Z_]* ;
  UpperID : [A-Z][a-zA-Z_]* ;
  Prim    : ( 'bit' | 'byte' ) 's'? ;
  INT     : [0-9]+ ;
  LineComment : '//' (~ NewLine)* NewLine ;
  NewLine : '\r'? '\n' ;
  WS      : [ \t\n\r\f\v] ;
|]

{-
data ChiselNTS = ChiselProd | ProdSimple | Magnitude | Alignment | Formals
  | Group | Tuple | Alt | Flags | SizeArith
  deriving (Eq, Ord, Enum, Show, Bounded, Hashable, Generic)
-}

instance Prettify ChiselNTSymbol where prettify = rshow
instance Prettify ChiselTSymbol where prettify = rshow

instance Ref ChiselNTSymbol where
  type Sym ChiselNTSymbol = ChiselNTSymbol
  getSymbol = id

-- (Name, Value) is what goes in the leaf of the AST...
--type ChiselTSymbol = Name
type ChiselAST = AST ChiselNTSymbol ChiselToken

type ChiselToken = T.Token ChiselTSymbol TokenValue

event2chisel :: ParseEvent ChiselAST ChiselNTSymbol ChiselToken -> ChiselAST
event2chisel e = D.trace (pshow e) (event2ast e)

isWhitespace (T.Token T_LineComment _) = True
isWhitespace (T.Token T_WS _) = True
isWhitespace _ = False

parse :: [ChiselToken] -> Maybe ChiselAST
parse = P.slrParse chisel event2chisel . filter (not . isWhitespace)

