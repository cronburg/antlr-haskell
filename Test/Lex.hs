module Main where
-- Allstar imports go here, e.g.:
-- import Text.ANTLR.Allstar.ATN (..)
import Text.ANTLR.Lex
import Text.ANTLR.Lex.Automata
import Text.ANTLR.Lex.NFA as NFA
import qualified Text.ANTLR.Lex.DFA as DFA

import Text.ANTLR.Lex.Regex

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
  (  validStartState nfa0
  && validFinalStates nfa0
  && validTransitions nfa0
  )
  @?=
  True

testClosureWith0 =
  closureWith (Edge 'a' ==) nfa0 (singleton 0)
  @?=
  fromList [0, 1]

testClosureWith1 =
  closureWith (NFAEpsilon ==) nfa0 (singleton 0)
  @?=
  fromList [0]

testClosureWith2 =
  closureWith (const True) nfa0 (singleton 0)
  @?=
  fromList [0, 1, 2, 3]

testClosureWith3 =
  closureWith (Edge 'b' ==) nfa0 (singleton 0)
  @?=
  fromList [0]

testMove0 =
  move nfa0 (fromList [0,1,2]) (Edge 'a')
  @?=
  fromList [0,1]

nfa334 :: NFA Char Int
nfa334 = Automata
  { _S = fL [0 .. 10]
  , _Σ = fL "ab"
  , s0 = 0
  , _F = fL [10]
  , _Δ = fL
    [ (0, NFAEpsilon, 1)
    , (0, NFAEpsilon, 7)
    , (1, NFAEpsilon, 2)
    , (1, NFAEpsilon, 4)
    , (2, Edge 'a', 3)
    , (3, NFAEpsilon, 6)
    , (4, Edge 'b', 5)
    , (5, NFAEpsilon, 6)
    , (6, NFAEpsilon, 1)
    , (6, NFAEpsilon, 7)
    , (7, Edge 'a', 8)
    , (8, Edge 'b', 9)
    , (9, Edge 'b', 10)
    ]
  }

_A = fromList [0,1,2,4,7]
_B = fromList [1,2,3,4,6,7,8]
_C = fromList [1,2,4,5,6,7]
_D = fromList [1,2,4,5,6,7,9]
_E = fromList [1,2,4,5,6,7,10]

a = 'a'
b = 'b'

dfa336 :: DFA.DFA Char (Set Int)
dfa336 = Automata
  { _S = fL [_A, _B, _C, _D, _E]
  , _Σ = fL "ab"
  , s0 = _A
  , _F = fL [_E]
  , _Δ = fL
    [ (_A, a, _B), (_A, b, _C)
    , (_B, a, _B), (_B, b, _D)
    , (_C, a, _B), (_C, b, _C)
    , (_D, a, _B), (_D, b, _E)
    , (_E, a, _B), (_E, b, _C)
    ]
  }

nfa2dfa0 =
 nfa2dfa nfa334
 @?=
 dfa336

nfa334Eps0 =
  NFA.epsClosure nfa334

epsilonNFA = 
  Automata
    { _S = fL [0, 1]
    , _Σ = fL ""
    , s0 = 0
    , _F = fL [1]
    , _Δ = fL [ (0, NFAEpsilon, 1) ]
    }

regexTest0 =
  regex2nfa Epsilon
  @?=
  epsilonNFA

regexTest1 =
  regex2nfa (Symbol 'a')
  @?=
  epsilonNFA { _Σ = fL "a", _Δ = fL [ (0, Edge 'a', 1) ] }

regexTestUnion =
  regex2nfa (Union (Symbol 'a') (Symbol 'b'))
  @?= Automata
    { _S = fL [0..5]
    , _Σ = fL "ab"
    , s0 = 4
    , _F = fL [5]
    , _Δ = fL [ (0, Edge 'a', 1)
              , (2, Edge 'b', 3)
              , (4, NFAEpsilon, 0)
              , (4, NFAEpsilon, 2)
              , (1, NFAEpsilon, 5)
              , (3, NFAEpsilon, 5) ]
    }

main :: IO ()
main = defaultMainWithOpts
  [ testCase "testValid0" testValid0
  , testCase "testClosureWith0" testClosureWith0
  , testCase "testClosureWith1" testClosureWith1
  , testCase "testClosureWith2" testClosureWith2
  , testCase "testClosureWith3" testClosureWith3
  , testCase "testMove0" testMove0
  , testCase "nfa2dfa0" nfa2dfa0
  , testCase "regexTest0" regexTest0
  , testCase "regexTest1" regexTest1
  , testCase "regexTestUnion" regexTestUnion
  ] mempty

