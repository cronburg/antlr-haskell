module Main where
import Test.Text.ANTLR.Allstar.Grammar
import Text.ANTLR.Allstar.Grammar
import Text.ANTLR.LL1

import Data.Set (fromList, union, empty)

import System.IO.Unsafe (unsafePerformIO)           
import Data.Monoid                                  
import Test.Framework                               
import Test.Framework.Providers.HUnit               
import Test.Framework.Providers.QuickCheck2         
import Test.HUnit                                   
import Test.QuickCheck (Property, quickCheck, (==>))
import qualified Test.QuickCheck.Monadic as TQM     

grm = dragonBook428

termination = first grm (NT "E") @?= first grm (NT "E")

firstF = first grm (NT "F") @?= fromList [Term "(", Term "id"]

noEps = first grm (NT "E") @?= fromList [Term "(", Term "id"]

foldEpsTest = foldWhileEpsilon union empty
  [ fromList [Term "(", Term "id"]
  , fromList [Term ")"]
  ]
  @?=
  fromList [Term "(", Term "id"]

main :: IO ()
main = defaultMainWithOpts
  [ testCase "fold_epsilon" foldEpsTest
  --, testCase "termination" termination
  --, testCase "no_epsilon" noEps
  , testCase "firstF" firstF
  ] mempty

