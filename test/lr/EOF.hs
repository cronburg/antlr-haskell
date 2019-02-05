{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, QuasiQuotes
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, FlexibleContexts, TemplateHaskell
    , DeriveDataTypeable #-}
module EOF where
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

import EOFGrammar

$(g4_parsers eOFAST eOFGrammar)

test_eof = case glrParse (== T_WS) "anything that is not a comma" of
  (ResultAccept ast) -> ast2words ast @?= ["anything", "that", "is", "not", "a", "comma"]
  (ResultSet xs)     -> assertFailure $ "Ambiguous parse: " ++ pshow' xs
  rest               -> assertFailure $ stripQuotesReadEscape $ "\"" ++ pshow' rest ++ "\""

