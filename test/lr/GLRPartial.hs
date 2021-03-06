{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, QuasiQuotes
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, FlexibleContexts, TemplateHaskell
    , DeriveDataTypeable #-}
module GLRPartial where
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

import GLRPartialGrammar

$(g4_parsers gLRPartialAST gLRPartialGrammar)

test_GLRPartial = case glrParse (== T_WS) "word\nword\nword\nw0rd" of
  (ResultAccept ast)         -> assertFailure $ "Was not suppose to parse: " ++ pshow' (ast2words ast)
  (ResultSet xs)             -> assertFailure $ "Ambiguous parse: " ++ pshow' xs                    
  (ErrorNoAction cfg asts _) -> return () -- Correct, should not have parsed
  rest                       -> assertFailure $ stripQuotesReadEscape $ "\"" ++ pshow' rest ++ "\""

