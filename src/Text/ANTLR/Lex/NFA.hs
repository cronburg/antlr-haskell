{-# LANGUAGE ScopedTypeVariables, MonadComprehensions #-}
module Text.ANTLR.Lex.NFA where
import Text.ANTLR.Lex.Automata
import Text.ANTLR.Lex.DFA (DFA(..))
import qualified Text.ANTLR.Lex.DFA as DFA

import Data.Set.Monad (singleton, notMember, union, Set(..), member)
import qualified Data.Set.Monad as Set

import Data.List (maximumBy)

data Edge s = Edge s | NFAEpsilon
  deriving (Ord, Eq, Show)

isEdge (Edge _) = True
isEdge _ = False

type NFA s i = Automata (Edge s) s i

type State i = i

type DFAState i = Config (State i)

epsClosure :: (Ord (State i), Ord s) => Automata (Edge s) s i -> Config i -> Config i
epsClosure = closureWith (NFAEpsilon ==)

-- Subset construction
nfa2dfa :: forall s i. (Ord s, Ord i)
  => NFA s i -> DFA s (Set (State i))
nfa2dfa nfa@Automata{s0 = s0, _Σ = _Σ, _F = _F0} = let
    
    epsCl = epsClosure nfa
    mv    = move nfa

    dS :: Config (DFAState i) -> Config (DFAState i) -> Set (Transition (DFA.Edge s) (DFAState i))
    dS marked ts
      | Set.null ts = Set.empty
      | otherwise = let
        
          _Δ  = [ (_T, a, epsCl (mv _T (Edge a)))
                | _T <- ts
                , _T `notMember` marked
                , a  <- _Σ
                ]
          _Us = Set.map (\(a,b,c) -> c) _Δ
          fromStates = Set.map (\(a,b,c) -> a) _Δ

        in _Δ `union` dS (fromStates `union` marked) _Us
    
    _Δ' :: Set (Transition (DFA.Edge s) (DFAState i))
    _Δ' = dS Set.empty (singleton s0')

    s0' = epsCl $ singleton s0

  in Automata
      { _S = Set.map tFrom _Δ' `union` Set.map tTo _Δ'
      , _Σ = _Σ
      , _Δ = _Δ'
      , s0 = s0'
      , _F = [nfaState | (_,_,nfaState) <- _Δ', c <- nfaState, c `member` _F0]
      }

allStates :: forall s i. (Ord i) => Set (Transition (Edge s) i) -> Set (State i)
allStates ts = [ n | (n, _, _) <- ts ] `union` [ n | (_, _, n) <- ts ]

{- Converts the given list of transitions into a complete NFA / Automata
 - structure, assuming two things:
 - * The first node of the first edge is the start state
 - * The last  node of the last  edge is the (only) final state
 -}
list2nfa :: forall s i. (Ord i, Ord s) => [Transition (Edge s) i] -> NFA s i
list2nfa [] = undefined
list2nfa ((t@(n1,_,_)):ts) = Automata
  { _S = allStates $ Set.fromList (t:ts)
  , _Σ = Set.fromList [ s | (_, Edge s, _) <- filter (\(_,e,_) -> isEdge e) (t:ts) ]
  , s0 = n1
  , _F = Set.fromList [ (\(_,_,c) -> c) $ last (t:ts) ]
  , _Δ = Set.fromList $ t:ts
  }

shiftAllStates :: forall s i. (Ord i, Ord s) => (i -> Int) -> (Int -> i) -> NFA s i -> NFA s i -> NFA s i
shiftAllStates from to
  n1 (n2@Automata{_Δ = _Δ2, _S = _S2, _F = _F2, s0 = s2_0})
  = n2 { _Δ = [ (to $ from i0 + shift, e, to $ from i1 + shift) | (i0, e, i1) <- _Δ2 ]
       , _S = [ to $ from i + shift | i <- _S2 ]
       , _F = [ to $ from i + shift | i <- _F2 ]
       , s0 = to $ from s2_0 + shift
       }
  where
    shift = 1 + foldr (\(i0, _, i1) i -> from $ maximum [to i, i0, i1]) 0 (_Δ n1)

nfaUnion :: forall s i. (Ord i, Ord s) => (i -> Int) -> (Int -> i) -> NFA s i -> NFA s i -> NFA s i
nfaUnion from to
  (n1@Automata{_Δ = _Δ1, _S = _S1, _F = _F1, s0 = s1_0}) n2
  = let

    Automata{_Δ = _Δ2, _S = _S2, _F = _F2, s0 = s2_0} = shiftAllStates from to n1 n2
    
    _Δ' =     _Δ1
      `union` _Δ2
      `union` Set.singleton (s0', NFAEpsilon, s1_0)
      `union` Set.singleton (s0', NFAEpsilon, s2_0)
      `union` [ (f1_0, NFAEpsilon, f0') | f1_0 <- _F1 ]
      `union` [ (f2_0, NFAEpsilon, f0') | f2_0 <- _F2 ]

    mx2 = 1 + foldr (\(i0, _, i1) i -> from $ maximum [to i, i0, i1]) 0 _Δ2

    s0' = to mx2
    f0' = to $ mx2 + 1

  in Automata
    { _S = allStates _Δ'
    , _Σ = [ e | (_, Edge e, _) <- Set.filter (\(_,e,_) -> isEdge e) _Δ' ]
    , s0 = s0'
    , _F = Set.fromList [f0']
    , _Δ = _Δ'
    }

nfaConcat :: forall s i. (Ord i, Ord s) => (i -> Int) -> (Int -> i) -> NFA s i -> NFA s i -> NFA s i
nfaConcat from to
  (n1@Automata{_Δ = _Δ1, _S = _S1, _F = _F1, s0 = s1_0}) n2
  = let
    Automata{_Δ = _Δ2, _S = _S2, _F = _F2, s0 = s2_0} = shiftAllStates from to n1 n2
    
    _Δ' =     _Δ1
      `union` _Δ2
      `union` [ (f1_0, NFAEpsilon, s2_0) | f1_0 <- _F1 ]
  
  in Automata
    { _S = allStates _Δ'
    , _Σ = [ e | (_, Edge e, _) <- Set.filter (\(_,e,_) -> isEdge e) _Δ' ]
    , s0 = s1_0
    , _F = _F2
    , _Δ = _Δ'
    }

nfaKleene = undefined

nfaPosclos = undefined

