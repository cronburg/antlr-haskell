{-# LANGUAGE ScopedTypeVariables, MonadComprehensions #-}
module Text.ANTLR.Lex.NFA where
import Text.ANTLR.Lex.Automata
import Text.ANTLR.Lex.DFA (DFA(..))
import qualified Text.ANTLR.Lex.DFA as DFA

import Data.Set.Monad (singleton, notMember, union, Set(..))
import qualified Data.Set.Monad as Set

data Edge s = Edge s | NFAEpsilon
  deriving (Ord, Eq)

type NFA s i = Automata (Edge s) s i

type State i = i

type DFAState i = Config (State i)

epsClosure :: (Ord (State i), Ord s) => Automata (Edge s) s i -> Config i -> Config i
epsClosure = closureWith (NFAEpsilon ==)

-- Subset construction
nfa2dfa :: forall s i. (Ord s, Ord i)
  => NFA s i -> DFA s (Set (State i))
nfa2dfa nfa@Automata{s0 = s0, _Σ = _Σ} = let
    
    epsCl = epsClosure nfa
    mv    = move nfa

    dS :: Config (DFAState i) -> Config (DFAState i) -> Set (Transition (DFA.Edge s) (DFAState i))
    dS busy ts
      | Set.null ts = Set.empty
      | otherwise = let
        
          _Δ  = [ (_T, a, epsCl (mv _T (Edge a)))
                | _T <- ts
                , _T `notMember` busy
                , a  <- _Σ
                ]
          _Us = Set.map (\(a,b,c) -> c) _Δ

        in _Δ `union` dS (_Us `union` busy) _Us
    
    _Δ' :: Set (Transition (DFA.Edge s) (DFAState i))
    _Δ' = dS Set.empty (singleton s0')

    s0' = epsCl $ singleton s0

  in Automata
      { _S = Set.map tFrom _Δ' `union` Set.map tTo _Δ'
      , _Σ = _Σ
      , _Δ = _Δ'
      , s0 = s0'
      , _F = Set.empty
      }

