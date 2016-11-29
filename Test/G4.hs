
module Main where
-- Allstar imports go here, e.g.:
-- import Text.Allstar.ATN (..)


import System.IO.Unsafe (unsafePerformIO)
import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck (Property, quickCheck, (==>))
import qualified Test.QuickCheck.Monadic as TQM

import Language.ANTLR4
import Text.Allstar.Grammar
import Test.Language.ANTLR4.G4 (g4_basic)

test_g4_basic = do
  let _ = g4_basic
  1 @?= 1




main :: IO ()
main = defaultMainWithOpts
  [ testCase "g4_basic_compilation" test_g4_basic
  ] mempty
