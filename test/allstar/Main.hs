module Main where

import System.IO.Unsafe (unsafePerformIO)
import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck (Property, quickCheck, (==>))
import qualified Test.QuickCheck.Monadic as TQM

import qualified AllStarTests
import qualified ConvertP
import ConvertP (ConvertIt(..))

convertP_simple =
  case ConvertP.allstarParse (const False) "STARTabc" of
    (Right ast) -> ConvertP.ast2root ast @=? (Start "abc")
    (Left err)  -> assertFailure err

convertP_simple2 =
  case ConvertP.allstarParse (const False) "ENDefg" of
    (Right ast) -> ConvertP.ast2root ast @=? (End 3)
    (Left err)  -> assertFailure err

tests =
  [ testCase "convertP_simple" convertP_simple
  , testCase "convertP_simple2" convertP_simple2
  ]

main :: IO ()
main = defaultMainWithOpts
  (AllStarTests.tests ++ tests)
  mempty

