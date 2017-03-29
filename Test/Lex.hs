module Main where
-- Allstar imports go here, e.g.:
-- import Text.ANTLR.Allstar.ATN (..)
import Text.ANTLR.Lex

import System.IO.Unsafe (unsafePerformIO)           
import Data.Monoid                                  
import Test.Framework                               
import Test.Framework.Providers.HUnit               
import Test.Framework.Providers.QuickCheck2         
import Test.HUnit                                   
import Test.QuickCheck (Property, quickCheck, (==>))
import qualified Test.QuickCheck.Monadic as TQM     

import Data.Set (fromList, Set(..))

fL :: Ord a => [a] -> Set a
fL = fromList

nfa0 :: NFA Char Int
nfa0 = NFA
  { _S = fL [0, 1, 2, 3]
  , _Σ = fL "ab"
  , s0 = 0
  , _F = fL [3]
  , _Δ = fL
    [ (0, 'a', fL [0,1])
    , (0, 'b', fL [0])
    , (1, 'b', fL [2])
    , (2, 'b', fL [3])
    ]
  }

testValid0 =
  True
  @?=
  (  validStartState nfa0
  && validFinalStates nfa0
  && validTransitions nfa0
  )

main :: IO ()
main = defaultMainWithOpts
  [ testCase "testValid0" testValid0
  ] mempty

