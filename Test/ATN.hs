module Main where
import Text.Allstar.ATN
import Test.Text.Allstar.ATN

import System.IO.Unsafe (unsafePerformIO)           
import Data.Monoid                                  
import Test.Framework                               
import Test.Framework.Providers.HUnit               
import Test.Framework.Providers.QuickCheck2         
import Test.HUnit                                   
import Test.QuickCheck (Property, quickCheck, (==>))
import qualified Test.QuickCheck.Monadic as TQM     

test_paperATNGrammar =
  atnOf paperATNGrammar
  @?=
  exp_paperATN

main :: IO ()
main = defaultMainWithOpts
  [ testCase "paper_ATN_Grammar" test_paperATNGrammar
  ] mempty

