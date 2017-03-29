module Main where
-- Allstar imports go here, e.g.:
-- import Text.ANTLR.Allstar.ATN (..)
import Text.ANTLR.Lex
import Text.ANTLR.Lex.Automata
import Text.ANTLR.Lex.NFA as NFA
import qualified Text.ANTLR.Lex.DFA as DFA

import System.IO.Unsafe (unsafePerformIO)           
import Data.Monoid                                  
import Test.Framework                               
import Test.Framework.Providers.HUnit               
import Test.Framework.Providers.QuickCheck2         
import Test.HUnit                                   
import Test.QuickCheck (Property, quickCheck, (==>))
import qualified Test.QuickCheck.Monadic as TQM     

import Data.Set.Monad (fromList, Set(..), singleton)

fL :: Ord a => [a] -> Set a
fL = fromList

nfa0 :: NFA Char Int
nfa0 = Automata
  { _S = fL [0, 1, 2, 3]
  , _Σ = fL "ab"
  , s0 = 0
  , _F = fL [3]
  , _Δ = fL
    [ (0, Edge 'a', 0)
    , (0, Edge 'a', 1)
    , (0, Edge 'b', 0)
    , (1, Edge 'b', 2)
    , (2, Edge 'b', 3)
    ]
  }

testValid0 =
  True
  @?=
  (  validStartState nfa0
  && validFinalStates nfa0
  && validTransitions nfa0
  )

testClosureWith0 =
  fromList [0, 1]
  @?=
  closureWith (Edge 'a' ==) (singleton 0) nfa0

testClosureWith1 =
  fromList []
  @?=
  closureWith (NFAEpsilon ==) (singleton 0) nfa0

testClosureWith2 =
  fromList [0, 1, 2, 3]
  @?=
  closureWith (const True) (singleton 0) nfa0

testClosureWith3 =
  fromList [0]
  @?=
  closureWith (Edge 'b' ==) (singleton 0) nfa0

main :: IO ()
main = defaultMainWithOpts
  [ testCase "testValid0" testValid0
  , testCase "testClosureWith0" testClosureWith0
  , testCase "testClosureWith1" testClosureWith1
  , testCase "testClosureWith2" testClosureWith2
  , testCase "testClosureWith3" testClosureWith3
  ] mempty

