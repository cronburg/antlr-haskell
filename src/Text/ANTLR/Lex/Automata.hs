{-# LANGUAGE ScopedTypeVariables, MonadComprehensions #-}
{-|
  Module      : Text.ANTLR.Automata
  Description : Automatons and algorithms as used during tokenization
  Copyright   : (c) Karl Cronburg, 2018
  License     : BSD3
  Maintainer  : karl@cs.tufts.edu
  Stability   : experimental
  Portability : POSIX

-}
module Text.ANTLR.Lex.Automata where
import Text.ANTLR.Set (Set(..), member, toList, union, notMember, Hashable(..), fromList)
import qualified Text.ANTLR.Set as Set

-- | An automaton with edges @e@, symbols @s@, and state indices @i@
data Automata e s i = Automata
  { _S :: Set i                  -- ^ Finite set of states.
  , _Σ :: Set s                  -- ^ Input (edge) alphabet
  , _Δ :: Set (Transition e i)   -- ^ Transition function
  , s0 :: i                      -- ^ Start state
  , _F :: Set i                  -- ^ Accepting states
  } deriving (Eq)

instance (Eq e, Eq s, Eq i, Hashable e, Hashable s, Hashable i, Show e, Show s, Show i) => Show (Automata e s i) where
  show (Automata s sigma delta s0 f) =
    show s
    ++ "\n  Σ:  " ++ show sigma
    ++ "\n  Δ:  " ++ show delta
    ++ "\n  s0: " ++ show s0
    ++ "\n  F:  " ++ show f
    ++ "\n"

-- | Edge label of an automaton, on which we traverse if we match
--   on one of the tokens @t@ in the set. The boolean is for negation
--   of the set.
type AutomataEdge t = (Bool, Set t)

-- | A triplet with an edge alphabet of @e@ and node states of @i@.
type Transition e i = (i, AutomataEdge e, i)

-- | The from-node component of a 'Transition'
tFrom :: Transition e i -> i
tFrom (a,b,c) = a

-- | The to-node component of a 'Transition'
tTo   :: Transition e i -> i
tTo (a,b,c) = c

-- | The set of edge characters in @e@ of a 'Transition'
tEdge :: Transition e i -> Set e
tEdge (a,(comp, b),c) = b

-- | Determine the edge-label alphabet of a set of transitions.
transitionAlphabet __Δ =
  [ e
  | (_, (c, es), _) <- toList __Δ
  , e               <- es
  ]

-- | Compress a set of transitions such that every pair of (start,end) states
--   appears at most once in the set.
compress ::
  (Eq i, Eq e, Hashable i, Hashable e)
  => Set (Transition e i) -> Set (Transition e i)
compress __Δ = fromList
  [ ( a, (c, fromList [ e
             | (a', (c', es'), b') <- toList __Δ
             , a' == a && b' == b && c' == c
             , e <- toList es'
             ])
    , b)
  | (a, (c, es), b) <- toList __Δ
  ]

-- | XOR helper function over booleans.
xor a b = (not a && b) || (not b && a)

-- | Is the given transition triplet (with a single @e@ character as the edge
--   edge label) in some set of transitions? Note that we need to handle complement
--   sets here, in case the given @e@ is in the complement of one of the
--   transitions in the set.
transitionMember ::
  (Eq i, Hashable e, Eq e)
  => (i, e, i) -> Set (Transition e i) -> Bool
transitionMember (a, e, b) _Δ =
  or
      [ xor complement (e `member` es)
      | (a', (complement, es), b') <- toList _Δ
      , a' == a
      , b' == b
      ]

-- | Is the given character @s@ accepted by the given edge label?
edgeMember s (complement, es) = xor complement (s `member` es)

-- | An automaton must either 'Accept' or 'Reject'.
data Result = Accept | Reject

-- | Is the start state valid?
validStartState nfa = s0 nfa `member` _S nfa

-- | Are all of the ending states valid?
validFinalStates nfa = and [s `member` _S nfa | s <- toList $ _F nfa]

-- | Can all of the nodes as defined by the set of transitions be found
--   in the set of allowable states '_S'?
validTransitions ::
  forall e s i. (Hashable e, Hashable i, Eq e, Eq i)
  => Automata e s i -> Bool
validTransitions nfa = let
    vT :: [Transition e i] -> Bool
    vT [] = True
    vT ((s1, es, s2):rest) =
         s1 `member` _S nfa
      && s2 `member` _S nfa
      && vT rest
  in vT $ (toList . _Δ) nfa

-- | An automaton configuration is the set of state (indices) your
--   can currently be in.
type Config i = Set i

-- | Generic closure function so that *someone* never asks "what's a closure?" ever
--   again. For an epsilon-closure the given @fncn@ needs to return 'True' when
--   given an @e@ that is an epsilon, and 'False' in all other cases.
closureWith
  :: forall e s i. (Hashable e, Hashable i, Eq e, Eq i)
  => (e -> Bool) -> Automata e s i -> Config i -> Config i
closureWith fncn Automata{_S = _S, _Δ = _Δ'} states = let

    -- Check which edges are "epsilons" (or something else).
    _Δ = Set.map (\(a,(comp, b),c) -> (a, (comp, Set.map fncn b), c)) _Δ'

    cl :: Config i -> Config i -> Config i
    cl busy ss
      | Set.null ss = Set.empty
      | otherwise = let
          ret = fromList
                [ s'  | s  <- toList ss
                      , s' <- toList _S
                      , s' `notMember` busy
                      , (s, True, s') `transitionMember` _Δ ]
        in ret `union` cl (ret `union` busy) ret
  in states `union` cl Set.empty states
  --in Set.foldr (\a b -> union (cl a) b) Set.empty states

-- | Consume the @e@ character given, based on the fact that we are currently
--   in some 'Config i' of states, resulting in a new config consisting of the
--   states that we can get to by doing so.
move
  :: forall e s i. (Hashable e, Hashable i, Eq i, Eq e)
  => Automata e s i -> Config i -> e -> Config i
move Automata{_S = _S, _Δ = _Δ} _T a = fromList
  [ s'  | s  <- toList _T
        , s' <- toList _S
        , (s, a, s') `transitionMember` _Δ ]

-- | Whether or not (a, (True, _), b) is a transition in our set of transitions.
complementMember
  :: (Hashable i, Eq i, Hashable e, Eq e)
  => (i, i) -> Set (Transition e i) -> Bool
complementMember (a, b) =
  not . null . Set.filter (\(a', (c, _), b') -> a' == a && b' == b && c)

-- | Set of states you can move to if you see a character not in the alphabet.
moveComplement
  :: forall e s i. (Hashable e, Hashable i, Eq i, Eq e)
  => Automata e s i -> Config i -> Config i
moveComplement Automata{_S = _S, _Δ = _Δ} _T = fromList
  [ s'  | s  <- toList _T
        , s' <- toList _S
        , (s, s') `complementMember` _Δ ]

