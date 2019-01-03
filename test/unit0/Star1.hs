{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, QuasiQuotes
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, FlexibleContexts, TemplateHaskell
    , DeriveDataTypeable #-}
module Star1 where
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
import Debug.Trace as D

import Star1Grammar

$(g4_parsers star1AST star1Grammar)

test_star1 = D.trace (pshow' star1Grammar) $
  case glrParse (== T_WS) "me page you { byte, byte }" of
    (ResultAccept ast) -> ast2words ast @?= Frst (Yep ("me", "page")) [] [Byte, Byte]
    rest -> assertFailure $ "Did not parse: " ++ pshow' rest

