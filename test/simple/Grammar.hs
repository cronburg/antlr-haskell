{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, QuasiQuotes
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, FlexibleContexts, TemplateHaskell
    , DeriveDataTypeable #-}
module Grammar where
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

data Attr = A | B

data Decl = Foo | Bar

[g4|
	grammar Simple;

	attrDecl : attr* decl ;

  attrDecl2 : attr? decl ;

  attrDecl3 : attr+ decl ;

	attr  : 'a' ';' -> A
        | 'b' ';' -> B
        ;

  decl  : 'foo' -> Foo
        | 'bar' -> Bar
        ;
  
  UNICODE : '\u0008' -> String ;

|]

