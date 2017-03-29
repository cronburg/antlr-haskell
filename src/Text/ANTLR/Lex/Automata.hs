{-# LANGUAGE ScopedTypeVariables, MonadComprehensions #-}
module Text.ANTLR.Lex.Automata where
import Data.Set.Monad (Set(..), member, toList, union, notMember)
import qualified Data.Set.Monad as Set

-- e = edge type, s = symbols, i = state indices
data Automata e s i = Automata
  { _S :: Set (State i)          -- Finite set of states.
  , _Σ :: Set s                  -- Input (edge) alphabet
  , _Δ :: Set (Transition e i)   -- Transition function
  , s0 :: State i                -- Start state
  , _F :: Set (State i)          -- Accepting states
  }

type Transition e i = (State i, e, State i)

type State i = i

data Result = Accept | Reject

validStartState nfa = s0 nfa `member` _S nfa

validFinalStates nfa = and [s `member` _S nfa | s <- toList $ _F nfa]

validTransitions :: forall e s i. (Ord e, Ord (State i)) => Automata e s i -> Bool
validTransitions nfa = let
    vT :: [Transition e i] -> Bool
    vT [] = True
    vT ((s1, e, s2):rest) =
         s1 `member` _S nfa
      && s2 `member` _S nfa
      && vT rest
  in vT $ (toList . _Δ) nfa

type Config i = Set (State i)

-- Generic closure function so that *someone* never asks "what's a closure?" ever
-- again.
closureWith
  :: forall e s i. (Ord e, Ord (State i))
  => (e -> Bool) -> Config i -> Automata e s i -> Config i
closureWith fncn states Automata{_S = _S, _Δ = _Δ'} = let
    
    -- Check which edges are "epsilons" (or something else).
    _Δ = Set.map (\(a,b,c) -> (a, fncn b, c)) _Δ'
    
    clOne :: State i -> Config i
    clOne st = Set.empty

    clMany :: Config i -> Config i -> Config i
    clMany busy ss
      | Set.null ss = Set.empty
      | otherwise = let
          ret = [ s'  | s  <- ss
                      , s' <- _S
                      , s' `notMember` busy
                      , (s, True, s') `member` _Δ ]
        in ret `union` clMany (ret `union` busy) ret
  in clMany Set.empty states
  --in Set.foldr (\a b -> union (cl a) b) Set.empty states

