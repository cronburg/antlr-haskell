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

import Text.ANTLR.Set (fromList, Set(..), singleton, Hashable)

fL :: (Hashable a, Eq a) => [a] -> Set a
fL = fromList

nfa0 :: NFA Char Int
nfa0 = Automata
  { _S = fL [0, 1, 2, 3]
  , _Σ = fL "ab"
  , s0 = 0
  , _F = fL [3]
  , _Δ = fL
    [ (0, singleton $ Edge 'a', 0)
    , (0, singleton $ Edge 'a', 1)
    , (0, singleton $ Edge 'b', 0)
    , (1, singleton $ Edge 'b', 2)
    , (2, singleton $ Edge 'b', 3)
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
    [ (0, singleton NFAEpsilon, 1)
    , (0, singleton NFAEpsilon, 7)
    , (1, singleton NFAEpsilon, 2)
    , (1, singleton NFAEpsilon, 4)
    , (2, singleton $ Edge 'a', 3)
    , (3, singleton NFAEpsilon, 6)
    , (4, singleton $ Edge 'b', 5)
    , (5, singleton NFAEpsilon, 6)
    , (6, singleton NFAEpsilon, 1)
    , (6, singleton NFAEpsilon, 7)
    , (7, singleton $ Edge 'a', 8)
    , (8, singleton $ Edge 'b', 9)
    , (9, singleton $ Edge 'b', 10)
    ]
  }

_A = fromList [0,1,2,4,7]
_B = fromList [1,2,3,4,6,7,8]
_C = fromList [1,2,4,5,6,7]
_D = fromList [1,2,4,5,6,7,9]
_E = fromList [1,2,4,5,6,7,10]

a = singleton 'a'
b = singleton 'b'

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
    , _Δ = fL [ (0, singleton NFAEpsilon, 1) ]
    }

regexTest0 =
  regex2nfa Epsilon
  @?=
  epsilonNFA

regexTest1 =
  regex2nfa (Symbol 'a')
  @?=
  epsilonNFA { _Σ = fL "a", _Δ = fL [ (0, singleton $ Edge 'a', 1) ] }

regexTestUnion =
  regex2nfa (Union (Symbol 'a') (Symbol 'b'))
  @?= Automata
    { _S = fL [0..5]
    , _Σ = fL "ab"
    , s0 = 4
    , _F = fL [5]
    , _Δ = fL [ (0, singleton $ Edge 'a', 1)
              , (2, singleton $ Edge 'b', 3)
              , (4, singleton NFAEpsilon, 0)
              , (4, singleton NFAEpsilon, 2)
              , (1, singleton NFAEpsilon, 5)
              , (3, singleton NFAEpsilon, 5) ]
    }

regexTestConcat =
  regex2nfa (Concat [Symbol 'a', Symbol 'b'])
  @?= Automata
    { _S = fL [0..3]
    , _Σ = fL "ab"
    , s0 = 0
    , _F = fL [3]
    , _Δ = fL [ (0, singleton $ Edge 'a', 1)
              , (1, singleton NFAEpsilon, 2)
              , (2, singleton $ Edge 'b', 3) ]
    }

regexTestKleene =
  regex2nfa (Kleene (Concat [Symbol 'a', Symbol 'b']))
  @?= Automata
    { _S = fL [0..5]
    , _Σ = fL "ab"
    , s0 = 4
    , _F = fL [5]
    , _Δ = fL [ (0, singleton $ Edge 'a', 1)
              , (1, singleton NFAEpsilon, 2)
              , (2, singleton $ Edge 'b', 3)
              , (4, singleton NFAEpsilon, 0)
              , (4, singleton NFAEpsilon, 5)
              , (3, singleton NFAEpsilon, 0)
              , (3, singleton NFAEpsilon, 5)]
    }

regexTestPosclos =
  regex2nfa (PosClos (Concat [Symbol 'a', Symbol 'b']))
  @?= Automata
    { _S = fL [0..9]
    , _Σ = fL "ab"
    , s0 = 0
    , _F = fL [9]
    , _Δ = fL [ (0, singleton $ Edge 'a', 1)
              , (1, singleton NFAEpsilon, 2)
              , (2, singleton $ Edge 'b', 3)
              , (3, singleton NFAEpsilon, 8)
              , (4, singleton $ Edge 'a', 5)
              , (5, singleton NFAEpsilon, 6)
              , (6, singleton $ Edge 'b', 7)
              , (8, singleton NFAEpsilon, 4)
              , (8, singleton NFAEpsilon, 9)
              , (7, singleton NFAEpsilon, 4)
              , (7, singleton NFAEpsilon, 9)]
    }

dfaABPlus = regex2dfa (PosClos (Concat [Symbol 'a', Symbol 'b']))
dfaWS     = regex2dfa (PosClos (Symbol ' '))

dfaGetName x
  | x == dfaWS     = "ws"
  | x == dfaABPlus = "ab+"
  | otherwise      = "ERROR"

tokenizeTest0 =
  tokenize [dfaABPlus, dfaWS] dfaGetName const "abab ab ababab"
  @?=
  [ Token "ab+" "abab"
  , Token "ws"  " "
  , Token "ab+" "ab"
  , Token "ws"  " "
  , Token "ab+" "ababab"
  , EOF
  ]

{-
dfaID = regex2dfa
  (PosClos $ MultiUnion
              [ Class ['a' .. 'z']
              , Class ['A' .. 'Z']
              , Symbol '_'
              ])
-}

dfaID = regex2dfa
  (PosClos $ Class $ '_' : ['a' .. 'z'] ++ ['A' .. 'Z'])

-- For profiling runtime of DFA subset construction (nfa2dfa):
dfaIDTest =
  dfaID
  @?=
  dfaID

dfaEQ   = regex2dfa (Symbol '=')
dfaSEMI = regex2dfa (Symbol ';')

dfaINT  = regex2dfa (PosClos $ Class [ '0' .. '9' ])

data TermSymbol = T_ID | T_INT | T_WS | T_EQ | T_SEMI
  deriving (Eq, Ord, Show)

dfa2symbol dfa
  | dfa == dfaWS   = T_WS
  | dfa == dfaID   = T_ID
  | dfa == dfaINT  = T_INT
  | dfa == dfaEQ   = T_EQ
  | dfa == dfaSEMI = T_SEMI
  | otherwise      = undefined -- Should never happen (always one of the DFAs given)

data TermValue =
    ID  String
  | INT Int
  | WS  String
  | EQSIGN
  | SEMI
  deriving (Eq, Ord, Show)

lexeme2value lexeme T_WS   = WS lexeme
lexeme2value lexeme T_ID   = ID lexeme
lexeme2value lexeme T_INT  = INT $ read lexeme
lexeme2value lexeme T_EQ   = EQSIGN
lexeme2value lexeme T_SEMI = SEMI

tokenizeTest1 =
  tokenize [dfaWS, dfaID, dfaINT, dfaEQ, dfaSEMI] dfa2symbol lexeme2value "_matt = 0;"
  @?=
  [ Token T_ID (ID "_matt")
  , Token T_WS (WS " ")
  , Token T_EQ EQSIGN
  , Token T_WS (WS " ")
  , Token T_INT (INT 0)
  , Token T_SEMI SEMI
  , EOF
  ]

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
  , testCase "regexTestConcat" regexTestConcat
  , testCase "regexTestKleene" regexTestKleene
  , testCase "regexTestPosclos" regexTestPosclos
  , testCase "tokenizeTest0" tokenizeTest0
  , testCase "dfaIDTest" dfaIDTest
  , testCase "tokenizeTest1" tokenizeTest1
  ] mempty

