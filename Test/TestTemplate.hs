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

main :: IO ()
main = defaultMainWithOpts
  [
  ] mempty

