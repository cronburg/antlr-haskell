{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, QuasiQuotes
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, FlexibleContexts #-}
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

data Attr = A | B

data Decl = Foo | Bar

[g4|
	grammar Simple;

	attrDecl : attr* decl ;

	attr  : 'a' ';' -> A
        | 'b' ';' -> B
        ;

  decl  : 'foo' -> Foo
        | 'bar' -> Bar
        ;

|]

test_star = () @?= ()

main :: IO ()
main = defaultMainWithOpts
  [ testCase "test_star" test_star
  ] mempty

