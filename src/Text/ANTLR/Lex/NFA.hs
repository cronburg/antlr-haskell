{-# LANGUAGE ScopedTypeVariables, MonadComprehensions, DeriveAnyClass,
             DeriveGeneric #-}
{-|
  Module      : Text.ANTLR.Lex.NFA
  Description : Nondeterministic finite automatons and algorithms to compute DFAs
  Copyright   : (c) Karl Cronburg, 2018
  License     : BSD3
  Maintainer  : karl@cs.tufts.edu
  Stability   : experimental
  Portability : POSIX

-}
module Text.ANTLR.Lex.NFA where
import Text.ANTLR.Lex.Automata
import Text.ANTLR.Lex.DFA (DFA(..))
import qualified Text.ANTLR.Lex.DFA as DFA

import Text.ANTLR.Set (singleton, notMember, union, Set(..), member, Hashable)
import qualified Text.ANTLR.Set as Set
import Text.ANTLR.Set (fromList, toList)

import Data.List (maximumBy)
import GHC.Generics (Generic)

-- | NFA edges can be labeled with either a symbol in symbol alphabet @s@,
--   or an epsilon.
data Edge s = Edge s | NFAEpsilon
  deriving (Ord, Eq, Hashable, Generic)

instance (Show s) => Show (Edge s) where
  show NFAEpsilon = "ϵ"
  show (Edge s)   = "E(" ++ show s ++ ")"

-- | Is this an edge (not an epsilon)?
isEdge :: Edge s -> Bool
isEdge (Edge _) = True
isEdge _        = False

-- | An NFA is an automata with edges @'Edge' s@ and nodes @i@.
type NFA s i = Automata (Edge s) s i

-- | NFA states
type State i = i

-- | DFA states as constructed from an NFA is a set (config) of NFA states.
type DFAState i = Config (State i)

-- | Epsilon closure of an NFA is a closure where we can traverse epsilons.
epsClosure ::
  (Ord i, Hashable i, Hashable s, Eq s)
  => Automata (Edge s) s i -> Config i -> Config i
epsClosure = closureWith (NFAEpsilon ==)

-- | Subset construction algorithm for constructing a DFA from an NFA.
nfa2dfa_slow :: forall s i. (Hashable s, Eq s, Hashable i, Eq i, Ord i)
  => NFA s i -> DFA s (Set (State i))
nfa2dfa_slow nfa@Automata{s0 = s0, _Σ = _Σ, _F = _F0} = let
    
    epsCl = epsClosure nfa
    mv    = move nfa

    dS :: Config (DFAState i) -> Config (DFAState i) -> Set (Transition (DFA.Edge s) (DFAState i))
    dS marked ts
      | Set.null ts = Set.empty
      | otherwise = let
        
          _Δ  = fromList
                [ (_T, (False, singleton a), epsCl (mv _T (Edge a)))
                | _T <- toList ts
                , _T `notMember` marked
                , a  <- toList _Σ
                ]

          _Us = Set.map (\(a,b,c) -> c) _Δ
          fromStates = Set.map (\(a,b,c) -> a) _Δ

        in _Δ `union` dS (fromStates `union` marked) _Us
    
    _Δ' :: Set (Transition (DFA.Edge s) (DFAState i))
    _Δ' = dS Set.empty (singleton s0')

    s0' = epsCl $ singleton s0

  in Automata
      { _S = fromList [ tFrom x | x <- toList _Δ' ] `union` fromList [ tTo x | x <- toList _Δ' ]
      , _Σ = _Σ
      , _Δ = _Δ'
      , s0 = s0'
      , _F = fromList [nfaState | (_,_,nfaState) <- toList _Δ', c <- toList nfaState, c `member` _F0]
      }

-- | Subset construction but where we compress our sets of transitions along the way.
nfa2dfa :: forall s i. (Hashable s, Eq s, Hashable i, Eq i, Ord i)
  => NFA s i -> DFA s (Set (State i))
nfa2dfa nfa@Automata{s0 = s0, _Σ = _Σ, _S = _S, _F = _F0} = let
    
    epsCl = epsClosure nfa
    mv    = move nfa

    dS :: Config (DFAState i) -> Config (DFAState i) -> Set (Transition (DFA.Edge s) (DFAState i))
    dS marked ts
      | Set.null ts = Set.empty
      | otherwise = let
        
          _Δ  =
            Set.fromList
                [ (_T, (False, singleton a), epsCl (mv _T (Edge a)))
                | _T <- Set.toList ts
                , _T `notMember` marked
                , a  <- Set.toList _Σ
                ]
            `union`
            Set.fromList
                [ (_T, (True, _Σ), epsCl $ moveComplement nfa _T)
                | _T <- Set.toList ts
                , _T `notMember` marked
                ]

          _Us = fromList [ c | (a,b,c) <- toList _Δ ]
          fromStates = fromList [ a | (a,b,c) <- toList _Δ ]

        in _Δ `union` dS (fromStates `union` marked) _Us
    
    _Δ' :: Set (Transition (DFA.Edge s) (DFAState i))
    _Δ' = let run_dS = dS Set.empty (singleton s0')
          in  Set.filter (\(_, _, b) -> not $ Set.null b) $ compress run_dS

    s0' = epsCl $ singleton s0

  in Automata
      { _S = fromList [ tFrom x | x <- toList _Δ' ] `union` fromList [ tTo x | x <- toList _Δ' ]
      , _Σ = _Σ
      , _Δ = _Δ'
      , s0 = s0'
      , _F = fromList [nfaState | (_,_,nfaState) <- toList _Δ', c <- toList nfaState, c `member` _F0]
      }

-- | Compute all the states statically used in a particular set of transitions.
allStates :: forall s i. (Hashable i, Eq i) => Set (Transition (Edge s) i) -> Set (State i)
allStates ts = fromList [ n | (n, _, _) <- toList ts ] `union` fromList [ n | (_, _, n) <- toList ts ]

-- | Converts the given list of transitions into a complete NFA / Automata
--   structure, assuming two things:
--
-- > The first node of the first edge is the start state
-- > The last  node of the last  edge is the (only) final state
--
list2nfa :: forall s i. (Hashable i, Eq i, Hashable s, Eq s) => [Transition (Edge s) i] -> NFA s i
list2nfa [] = undefined
list2nfa ((t@(n1,_,_)):ts) = Automata
  { _S = allStates $ Set.fromList (t:ts)
  , _Σ = Set.fromList [ e
          | (_, es, _) <- t:ts
          , Edge e     <- filter isEdge (Set.toList $ snd es)
          ]
  , s0 = n1
  , _F = Set.fromList [ (\(_,_,c) -> c) $ last (t:ts) ]
  , _Δ = Set.fromList $ t:ts
  }

-- | Rename the states in the second NFA such that they start at the index
--   one greater than the maximum index of the first NFA.
shiftAllStates ::
  forall s i. (Hashable i, Eq i, Ord i, Hashable s, Eq s)
  => (i -> Int) -> (Int -> i) -> NFA s i -> NFA s i -> NFA s i
shiftAllStates from to
  n1 (n2@Automata{_Δ = _Δ2, _S = _S2, _F = _F2, s0 = s2_0})
  = n2 { _Δ = fromList [ (to $ from i0 + shift, e, to $ from i1 + shift) | (i0, e, i1) <- toList _Δ2 ]
       , _S = fromList [ to $ from i + shift | i <- toList _S2 ]
       , _F = fromList [ to $ from i + shift | i <- toList _F2 ]
       , s0 = to $ from s2_0 + shift
       }
  where
    shift = 1 + foldr (\(i0, _, i1) i -> from $ maximum [to i, i0, i1]) 0 (_Δ n1)

-- | Take the union of two NFAs, renaming states according to 'shiftAllStates'.
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
      `union` Set.singleton (s0', (False, singleton NFAEpsilon), s1_0)
      `union` Set.singleton (s0', (False, singleton NFAEpsilon), s2_0)
      `union` fromList [ (f1_0, (False, singleton NFAEpsilon), f0') | f1_0 <- toList _F1 ]
      `union` fromList [ (f2_0, (False, singleton NFAEpsilon), f0') | f2_0 <- toList _F2 ]

    s0' = to mx2
    f0' = to $ mx2 + 1

  in Automata
    { _S = allStates _Δ'
    , _Σ =  fromList [ e
            | (_, es, _)  <- toList _Δ'
            , Edge e      <- toList $ Set.filter isEdge $ snd es
            ]
    , s0 = s0'
    , _F = Set.fromList [f0']
    , _Δ = _Δ'
    }

-- | Concatenate two NFAs, renaming states in the second NFA according to 'shiftAllStates'.
nfaConcat ::
  forall s i. (Hashable i, Eq i, Ord i, Hashable s, Eq s) => (i -> Int) -> (Int -> i) -> NFA s i -> NFA s i -> NFA s i
nfaConcat from to
  (n1@Automata{_Δ = _Δ1, _S = _S1, _F = _F1, s0 = s1_0}) n2
  = let
    Automata{_Δ = _Δ2, _S = _S2, _F = _F2, s0 = s2_0} = shiftAllStates from to n1 n2
    
    _Δ' =     _Δ1
      `union` _Δ2
      `union` fromList [ (f1_0, (False, singleton NFAEpsilon), s2_0) | f1_0 <- toList _F1 ]
  
  in Automata
    { _S = allStates _Δ'
    , _Σ =  fromList [ e
            | (_, es, _)  <- toList _Δ'
            , Edge e      <- toList $ Set.filter isEdge $ snd es
            ]
    , s0 = s1_0
    , _F = _F2
    , _Δ = _Δ'
    }

-- | Take the Kleene-star of an NFA, adding epsilons as needed.
nfaKleene :: forall s i. (Ord i, Hashable i, Eq i, Hashable s, Eq s) => (i -> Int) -> (Int -> i) -> NFA s i -> NFA s i
nfaKleene from to 
  (n1@Automata{_Δ = _Δ1, _S = _S1, _F = _F1, s0 = s1_0})
  = let
    mx1 = 1 + foldr (\(i0, _, i1) i -> from $ maximum [to i, i0, i1]) 0 _Δ1

    s0' = to mx1
    f0' = to $ mx1 + 1

    _Δ' =     _Δ1
      `union` Set.singleton (s0', (False, singleton NFAEpsilon), s1_0)
      `union` Set.singleton (s0', (False, singleton NFAEpsilon), f0')
      `union` fromList [ (f1_0, (False, singleton NFAEpsilon), s1_0) | f1_0 <- toList _F1 ]
      `union` fromList [ (f1_0, (False, singleton NFAEpsilon), f0')  | f1_0 <- toList _F1 ]

  in Automata
    { _S = allStates _Δ'
    , _Σ =  fromList [ e
            | (_, es, _)  <- toList _Δ'
            , Edge e      <- toList $ Set.filter isEdge $ snd es
            ]
    , s0 = s0'
    , _F = Set.fromList [f0']
    , _Δ = _Δ'
    }

