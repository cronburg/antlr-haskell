{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, QuasiQuotes
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, FlexibleContexts, TemplateHaskell
    , DeriveDataTypeable #-}
module Main where
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

import PlusBug0
$(g4_parsers plusBug0Grammar plusBug0AST)

test_plusBug0 = case glrParse (== T_WS) "foo bar baz" of
  (ResultAccept ast) -> ast2plus ast @?= Plus [ "foo", "bar", "baz" ]
  _ -> assertFailure "Ambiguous parse"

main :: IO ()
main = defaultMainWithOpts
  [ testCase "test_plusBug0" test_plusBug0
  ] mempty

