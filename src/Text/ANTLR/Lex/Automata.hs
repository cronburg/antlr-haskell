{-# LANGUAGE ScopedTypeVariables, MonadComprehensions #-}
module Text.ANTLR.Lex.Automata where
import Data.Set.Monad (Set(..), member, toList, union, notMember)
import qualified Data.Set.Monad as Set

-- e = edge type, s = symbols, i = state indices
data Automata e s i = Automata
  { _S :: Set i                  -- Finite set of states.
  , _Σ :: Set s                  -- Input (edge) alphabet
  , _Δ :: Set (Transition e i)   -- Transition function
  , s0 :: i                      -- Start state
  , _F :: Set i                  -- Accepting states
  } deriving (Eq)

instance (Eq e, Eq s, Eq i, Ord e, Ord s, Ord i, Show e, Show s, Show i) => Show (Automata e s i) where
  show (Automata s sigma delta s0 f) =
    show s
    ++ "\n  Σ:  " ++ show sigma
    ++ "\n  Δ:  " ++ show delta
    ++ "\n  s0: " ++ show s0
    ++ "\n  F:  " ++ show f

type Transition e i = (i, e, i)

tFrom :: Transition e i -> i
tFrom (a,b,c) = a

tTo   :: Transition e i -> i
tTo (a,b,c) = c

tEdge :: Transition e i -> e
tEdge (a,b,c) = b

data Result = Accept | Reject

validStartState nfa = s0 nfa `member` _S nfa

validFinalStates nfa = and [s `member` _S nfa | s <- toList $ _F nfa]

validTransitions :: forall e s i. (Ord e, Ord i) => Automata e s i -> Bool
validTransitions nfa = let
    vT :: [Transition e i] -> Bool
    vT [] = True
    vT ((s1, e, s2):rest) =
         s1 `member` _S nfa
      && s2 `member` _S nfa
      && vT rest
  in vT $ (toList . _Δ) nfa

type Config i = Set i

-- Generic closure function so that *someone* never asks "what's a closure?" ever
-- again.
closureWith
  :: forall e s i. (Ord e, Ord i)
  => (e -> Bool) -> Automata e s i -> Config i -> Config i
closureWith fncn Automata{_S = _S, _Δ = _Δ'} states = let

    -- Check which edges are "epsilons" (or something else).
    _Δ = Set.map (\(a,b,c) -> (a, fncn b, c)) _Δ'

    cl :: Config i -> Config i -> Config i
    cl busy ss
      | Set.null ss = Set.empty
      | otherwise = let
          ret = [ s'  | s  <- ss
                      , s' <- _S
                      , s' `notMember` busy
                      , (s, True, s') `member` _Δ ]
        in ret `union` cl (ret `union` busy) ret
  in states `union` cl Set.empty states
  --in Set.foldr (\a b -> union (cl a) b) Set.empty states

move
  :: forall e s i. (Ord e, Ord i)
  => Automata e s i -> Config i -> e -> Config i
move Automata{_S = _S, _Δ = _Δ} _T a =
  [ s'  | s  <- _T
        , s' <- _S
        , (s, a, s') `member` _Δ ]

