{-# LANGUAGE QuasiQuotes, TemplateHaskell, ScopedTypeVariables #-}
module Main where
-- Project imports go here, e.g.:
import Language.Chisel.Tokenizer
import Text.ANTLR.Lex.Tokenizer (Token(..))
import Language.ANTLR4.FileOpener (open)

import System.IO.Unsafe (unsafePerformIO)
import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck (Property, quickCheck, (==>))
import qualified Test.QuickCheck.Monadic as TQM

chi = id

tokenizeGHC_val = tokenize [open| Test/Language/Chisel/Examples/GHC.chi |]

tokenizeGHC =
  tokenizeGHC_val
  @?=
  [ upperID "Heap", ws " ", lowerID "m", ws " ", lowerID "k", ws " ", arrow
  , ws "\n  ", pound, upperID "MegaBlock", vertbar, int 2
  ]

main :: IO ()
main = defaultMainWithOpts
  [ testCase "Tokenize GHC" tokenizeGHC
  ] mempty

