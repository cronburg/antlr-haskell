{-# LANGUAGE ScopedTypeVariables, MonadComprehensions, DeriveAnyClass,
             DeriveGeneric #-}
module Text.ANTLR.Lex.NFA where
import Text.ANTLR.Lex.Automata
import Text.ANTLR.Lex.DFA (DFA(..))
import qualified Text.ANTLR.Lex.DFA as DFA

import Text.ANTLR.Set (singleton, notMember, union, Set(..), member, Hashable)
import qualified Text.ANTLR.Set as Set

import Data.List (maximumBy)
import GHC.Generics (Generic)

data Edge s = Edge s | NFAEpsilon
  deriving (Ord, Eq, Show, Hashable, Generic)

isEdge (Edge _) = True
isEdge _ = False

type NFA s i = Automata (Edge s) s i

type State i = i

type DFAState i = Config (State i)

epsClosure ::
  (Ord i, Hashable i, Hashable s, Eq s)
  => Automata (Edge s) s i -> Config i -> Config i
epsClosure = closureWith (NFAEpsilon ==)

-- Subset construction
nfa2dfa_slow :: forall s i. (Hashable s, Eq s, Hashable i, Eq i, Ord i)
  => NFA s i -> DFA s (Set (State i))
nfa2dfa_slow nfa@Automata{s0 = s0, _Σ = _Σ, _F = _F0} = let
    
    epsCl = epsClosure nfa
    mv    = move nfa

    dS :: Config (DFAState i) -> Config (DFAState i) -> Set (Transition (DFA.Edge s) (DFAState i))
    dS marked ts
      | Set.null ts = Set.empty
      | otherwise = let
        
          _Δ  = [ (_T, singleton a, epsCl (mv _T (Edge a)))
                | _T <- ts
                , _T `notMember` marked
                , a  <- _Σ
                ]
          _Us = [ c | (a,b,c) <- _Δ ]
          fromStates = [ a | (a,b,c) <- _Δ ]

        in _Δ `union` dS (fromStates `union` marked) _Us
    
    _Δ' :: Set (Transition (DFA.Edge s) (DFAState i))
    _Δ' = dS Set.empty (singleton s0')

    s0' = epsCl $ singleton s0

  in Automata
      { _S = [ tFrom x | x <- _Δ' ] `union` [ tTo x | x <- _Δ' ]
      , _Σ = _Σ
      , _Δ = _Δ'
      , s0 = s0'
      , _F = [nfaState | (_,_,nfaState) <- _Δ', c <- nfaState, c `member` _F0]
      }

nfa2dfa :: forall s i. (Hashable s, Eq s, Hashable i, Eq i, Ord i)
  => NFA s i -> DFA s (Set (State i))
nfa2dfa nfa@Automata{s0 = s0, _Σ = _Σ, _F = _F0} = let
    
    epsCl = epsClosure nfa
    mv    = move nfa

    dS :: Config (DFAState i) -> Config (DFAState i) -> Set (Transition (DFA.Edge s) (DFAState i))
    dS marked ts
      | Set.null ts = Set.empty
      | otherwise = let
        
          _Δ  = Set.fromList
                [ (_T, singleton a, epsCl (mv _T (Edge a)))
                | _T <- Set.toList ts
                , _T `notMember` marked
                , a  <- Set.toList _Σ
                ]
          _Us = [ c | (a,b,c) <- _Δ ]
          fromStates = [ a | (a,b,c) <- _Δ ]

        in _Δ `union` dS (fromStates `union` marked) _Us
    
    _Δ' :: Set (Transition (DFA.Edge s) (DFAState i))
    _Δ' = dS Set.empty (singleton s0')

    s0' = epsCl $ singleton s0

  in Automata
      { _S = [ tFrom x | x <- _Δ' ] `union` [ tTo x | x <- _Δ' ]
      , _Σ = _Σ
      , _Δ = compress _Δ'
      , s0 = s0'
      , _F = [nfaState | (_,_,nfaState) <- _Δ', c <- nfaState, c `member` _F0]
      }

allStates :: forall s i. (Hashable i, Eq i) => Set (Transition (Edge s) i) -> Set (State i)
allStates ts = [ n | (n, _, _) <- ts ] `union` [ n | (_, _, n) <- ts ]

{- Converts the given list of transitions into a complete NFA / Automata
 - structure, assuming two things:
 - * The first node of the first edge is the start state
 - * The last  node of the last  edge is the (only) final state
 -}
list2nfa :: forall s i. (Hashable i, Eq i, Hashable s, Eq s) => [Transition (Edge s) i] -> NFA s i
list2nfa [] = undefined
list2nfa ((t@(n1,_,_)):ts) = Automata
  { _S = allStates $ Set.fromList (t:ts)
  , _Σ = Set.fromList [ e
          | (_, es, _) <- t:ts
          , Edge e     <- filter isEdge (Set.toList es)
          ]
  , s0 = n1
  , _F = Set.fromList [ (\(_,_,c) -> c) $ last (t:ts) ]
  , _Δ = Set.fromList $ t:ts
  }

shiftAllStates ::
  forall s i. (Hashable i, Eq i, Ord i, Hashable s, Eq s)
  => (i -> Int) -> (Int -> i) -> NFA s i -> NFA s i -> NFA s i
shiftAllStates from to
  n1 (n2@Automata{_Δ = _Δ2, _S = _S2, _F = _F2, s0 = s2_0})
  = n2 { _Δ = [ (to $ from i0 + shift, e, to $ from i1 + shift) | (i0, e, i1) <- _Δ2 ]
       , _S = [ to $ from i + shift | i <- _S2 ]
       , _F = [ to $ from i + shift | i <- _F2 ]
       , s0 = to $ from s2_0 + shift
       }
  where
    shift = 1 + foldr (\(i0, _, i1) i -> from $ maximum [to i, i0, i1]) 0 (_Δ n1)

nfaUnion ::
  forall s i. (Ord i, Hashable i, Eq i, Hashable s, Eq s)
  => (i -> Int) -> (Int -> i) -> NFA s i -> NFA s i -> NFA s i
nfaUnion from to
  (n1@Automata{_Δ = _Δ1, _S = _S1, _F = _F1, s0 = s1_0}) n2
  = let

    Automata{_Δ = _Δ2, _S = _S2, _F = _F2, s0 = s2_0} = shiftAllStates from to n1 n2
    mx2 = 1 + foldr (\(i0, _, i1) i -> from $ maximum [to i, i0, i1]) 0 _Δ2

    _Δ' =     _Δ1
      `union` _Δ2
      `union` Set.singleton (s0', singleton NFAEpsilon, s1_0)
      `union` Set.singleton (s0', singleton NFAEpsilon, s2_0)
      `union` [ (f1_0, singleton NFAEpsilon, f0') | f1_0 <- _F1 ]
      `union` [ (f2_0, singleton NFAEpsilon, f0') | f2_0 <- _F2 ]

    s0' = to mx2
    f0' = to $ mx2 + 1

  in Automata
    { _S = allStates _Δ'
    , _Σ =
            [ e
            | (_, es, _) <- _Δ'
            , Edge e     <- Set.filter isEdge es
            ]
    , s0 = s0'
    , _F = Set.fromList [f0']
    , _Δ = _Δ'
    }

nfaConcat ::
  forall s i. (Hashable i, Eq i, Ord i, Hashable s, Eq s) => (i -> Int) -> (Int -> i) -> NFA s i -> NFA s i -> NFA s i
nfaConcat from to
  (n1@Automata{_Δ = _Δ1, _S = _S1, _F = _F1, s0 = s1_0}) n2
  = let
    Automata{_Δ = _Δ2, _S = _S2, _F = _F2, s0 = s2_0} = shiftAllStates from to n1 n2
    
    _Δ' =     _Δ1
      `union` _Δ2
      `union` [ (f1_0, singleton NFAEpsilon, s2_0) | f1_0 <- _F1 ]
  
  in Automata
    { _S = allStates _Δ'
    , _Σ =
            [ e
            | (_, es, _) <- _Δ'
            , Edge e     <- Set.filter isEdge es
            ]
    , s0 = s1_0
    , _F = _F2
    , _Δ = _Δ'
    }

nfaKleene :: forall s i. (Ord i, Hashable i, Eq i, Hashable s, Eq s) => (i -> Int) -> (Int -> i) -> NFA s i -> NFA s i
nfaKleene from to 
  (n1@Automata{_Δ = _Δ1, _S = _S1, _F = _F1, s0 = s1_0})
  = let
    mx1 = 1 + foldr (\(i0, _, i1) i -> from $ maximum [to i, i0, i1]) 0 _Δ1

    s0' = to mx1
    f0' = to $ mx1 + 1

    _Δ' =     _Δ1
      `union` Set.singleton (s0', singleton NFAEpsilon, s1_0)
      `union` Set.singleton (s0', singleton NFAEpsilon, f0')
      `union` [ (f1_0, singleton NFAEpsilon, s1_0) | f1_0 <- _F1 ]
      `union` [ (f1_0, singleton NFAEpsilon, f0')  | f1_0 <- _F1 ]

  in Automata
    { _S = allStates _Δ'
    , _Σ =
            [ e
            | (_, es, _) <- _Δ'
            , Edge e     <- Set.filter isEdge es
            ]
    , s0 = s0'
    , _F = Set.fromList [f0']
    , _Δ = _Δ'
    }

