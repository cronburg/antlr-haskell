module Text.Allstar.ATN where
-- Augmented Transition Network
import Text.Allstar.Grammar
import Text.Allstar.GSS

import Data.Set (Set(..), empty, fromList, toList)

-- ATN State
data ATNState = ATNState
  { isS   :: Bool
  , isF   :: Bool
  , atnId :: (NonTerminal, Int, Int)
  } deriving (Eq, Ord, Show)

type Gamma = GSS ATNState

data ATN s = ATN
  { q  :: Set ATNState
  -- Σ is an alphabet consisting of distinct elements which are comparable for
  -- equality.
  , _Σ :: Set (Edge s)
  , _Δ :: [Transition s]
  }

-- Tuple corresponding to a distinct transition in the ATN:
type Transition s = (ATNState, Edge s, ATNState)

-- The possible subscripts from Figure 8 of the ALL(*) paper
data StateSubscript = Start
                    | Middle (Int,Int)
                    | Accept

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

-- atnOf :: Grammar -> (ATNState,Edge) -> Maybe ATNState
atnOf :: Grammar s -> ATN s
atnOf g = let
    _Δ :: Production -> [(ATNState, Edge s, ATNState)]
    _Δ (Prod lhs rhs) = let
      --_Δ' ::
      _Δ' i (Left nt) = undefined
      _Δ' i (Right t) = undefined
      in zipWith _Δ' [0..length rhs] rhs
    in ATN
      { q  = empty
      , _Δ = concatMap _Δ $ (toList . ps) g
      , _Σ = empty
      }

--atnOf (ATNState{isS = s, isF = f, atnId = i}, NTE nte) = undefined
--atnOf g (ATNState{isS = s, isF = f, atnId = i}, TE te  ) = undefined
--atnOf g (ATNState{isS = s, isF = f, atnId = i}, PE pe  ) = undefined
--atnOf g (ATNState{isS = s, isF = f, atnId = i}, ME me  ) = undefined
--atnOf g (ATNState{isS = s, isF = f, atnId = i}, Epsilon) = undefined

