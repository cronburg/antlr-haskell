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
import Language.ANTLR4.Syntax (stripQuotesReadEscape)

import GLRIncGrammar

$(g4_parsers gLRIncAST gLRIncGrammar)

test_GLRInc = case glrParse (== T_WS) "word - foo" of
  (ResultAccept ast) -> ast2plus ast @?= Minus "word" "foo"
  (ResultSet xs)     -> assertFailure $ "Ambiguous parse: " ++ pshow' xs                    
  rest               -> assertFailure $ stripQuotesReadEscape $ "\"" ++ pshow' rest ++ "\""

