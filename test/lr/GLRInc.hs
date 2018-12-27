{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, QuasiQuotes
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, FlexibleContexts, TemplateHaskell
    , DeriveDataTypeable #-}
module GLRInc where
import Language.ANTLR4

import System.IO.Unsafe (unsafePerformIO)
import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck (Property, quickCheck, (==>))
import qualified Test.QuickCheck.Monadic as TQM

import Language.Haskell.TH.Syntax (lift)
import qualified Text.ANTLR.LR as LR
import Language.ANTLR4.Syntax (stripQuotesReadEscape)

data Plus = Plus String String | Minus String String
  deriving (Eq, Show)

[g4|
  grammar GLRInc;

  plus  : LowerID '+' Prim     -> Plus
        | Prim    '-' LowerID  -> Minus
        ;

  LowerID : [a-z][a-zA-Z0-9_]* -> String;
  Prim    : 'word'             -> String;

  WS      : [ \t\n\r\f\v]+     -> String;
|]

test_GLRInc = case glrParse (== T_WS) "word - foo" of
  (ResultAccept ast) -> ast2plus ast @?= Minus "word" "foo"
  (ResultSet xs)     -> assertFailure $ "Ambiguous parse: " ++ pshow' xs                    
  rest               -> assertFailure $ stripQuotesReadEscape $ "\"" ++ pshow' rest ++ "\""

