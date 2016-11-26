module Text.Allstar.ATN where
-- Augmented Transition Network
import Text.Allstar.Grammar
import Text.Allstar.GSS

import Data.Set (Set(..), empty, fromList, toList)

type Gamma = GSS ATNState

data ATN s = ATN
  -- q  :: Set ATNState
  -- Σ is an alphabet consisting of distinct elements which are comparable for
  -- equality.
  -- _Σ :: Set (Edge s)
  { _Δ :: Set (Transition s)
  } deriving (Eq, Ord, Show)

-- Tuple corresponding to a distinct transition in the ATN:
type Transition s = (ATNState, Edge s, ATNState)

-- The possible subscripts from Figure 8 of the ALL(*) paper
data ATNState = Start  NonTerminal
              | Middle NonTerminal Int Int
              | Accept NonTerminal
  deriving (Eq, Ord, Show)

sigma :: ATN s -> [Edge s]
sigma = undefined

e :: ATN s -> Set ATNState
e = undefined

f :: ATN s -> Set ATNState
f = undefined

data Edge s = NTE NonTerminal
            | TE  Terminal
            | PE  (Predicate s)
            | ME  (Mutator   s)
            | Epsilon
  deriving (Eq, Ord, Show)

-- atnOf :: Grammar -> (ATNState,Edge) -> Maybe ATNState
atnOf :: Grammar s -> ATN s
atnOf g = let
  
  _Δ :: Int -> Production s -> [Transition s]
  _Δ i (Production lhs (Prod _α)) = let
    
    -- The epsilon transitions for accept and final:
    sϵ = (Start lhs, Epsilon, Middle lhs i 0)
    fϵ = (Middle lhs i (length _α), Epsilon, Accept lhs)

    -- Construct an internal production state from the given ATN identifier
    st :: NonTerminal -> Int -> Int -> ATNState
    st = Middle
    
    -- Create the transition for the k^th production element in the i^th
    -- production:
    _Δ' :: Int -> ProdElem -> Transition s
    _Δ' k (NT nt) = (st lhs i (k - 1), NTE nt, st lhs i k)
    _Δ' k (T  t)  = (st lhs i (k - 1), TE  t,  st lhs i k)

    in [sϵ,fϵ] ++ zipWith _Δ' [1..(length _α)] _α

  _Δ i (Production lhs (Sem predicate _α)) = undefined
  _Δ i (Production lhs mutatorFncn) = undefined
  
  in ATN
    { _Δ = fromList $ concat $ zipWith _Δ [0..length (ps g)] $ ps g
    }

--atnOf (ATNState{isS = s, isF = f, stId = i}, NTE nte) = undefined
--atnOf g (ATNState{isS = s, isF = f, stId = i}, TE te  ) = undefined
--atnOf g (ATNState{isS = s, isF = f, stId = i}, PE pe  ) = undefined
--atnOf g (ATNState{isS = s, isF = f, stId = i}, ME me  ) = undefined
--atnOf g (ATNState{isS = s, isF = f, stId = i}, Epsilon) = undefined

