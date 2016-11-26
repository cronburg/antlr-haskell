module Main where
import Text.Allstar.ATN
import Test.Text.Allstar.ATN
import Data.Set (fromList, (\\))

import System.IO.Unsafe (unsafePerformIO)           
import Data.Monoid                                  
import Test.Framework                               
import Test.Framework.Providers.HUnit               
import Test.Framework.Providers.QuickCheck2         
import Test.HUnit                                   
import Test.QuickCheck (Property, quickCheck, (==>))
import qualified Test.QuickCheck.Monadic as TQM     

-- ATNs should be same:
test_paperATNGrammar =
  atnOf paperATNGrammar
  @?=
  exp_paperATN

-- Set difference to make debugging easier:
test_paperATNGrammar2 =
  ((_Δ . atnOf) paperATNGrammar \\ _Δ exp_paperATN)
  @?=
  fromList []

main :: IO ()
main = defaultMainWithOpts
  [ testCase "paper_ATN_Grammar"  test_paperATNGrammar
  , testCase "paper_ATN_Grammar2" test_paperATNGrammar2
  ] mempty

