module Main where
import Text.ANTLR.Allstar.ATN
import Test.Text.ANTLR.Allstar.ATN
import Data.Set.Monad (fromList, (\\))

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

test_addPredicates =
  atnOf addPredicates
  @?=
  exp_addPredicates

test_addPredicates2 =
  ((_Δ . atnOf) addPredicates \\ _Δ exp_addPredicates)
  @?=
  fromList []

test_addMutators =
  atnOf addMutators
  @?=
  exp_addMutators

main :: IO ()
main = defaultMainWithOpts
  [ testCase "paper_ATN_Grammar"  test_paperATNGrammar
  , testCase "paper_ATN_Grammar2" test_paperATNGrammar2
  , testCase "paper_ATN_Predicates2" test_addPredicates2
  , testCase "paper_ATN_Predicates" test_addPredicates
  , testCase "paper_ATN_Mutators" test_addMutators
  ] mempty

