{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, QuasiQuotes
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, FlexibleContexts, TemplateHaskell
    , DeriveDataTypeable #-}
module Star0 where
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

import Star0Grammar

$(g4_parsers star0AST star0Grammar)

test_star0 = case glrParse (== T_WS) "page page" of
  (ResultAccept ast) -> ast2words ast @?= ["page", "page"]
  rest -> assertFailure $ "Did not parse: " ++ pshow' rest

