module Text.Allstar.ATN where
-- Augmented Transition Network
import Text.Allstar.Grammar
import Text.Allstar.GSS

import Data.Set (Set(..), empty)

-- ATN State
data ATNState = ATNState
  { isS   :: Bool
  , isF   :: Bool
  , atnId :: Int
  }

type Gamma = GSS ATNState

type ATN s = (ATNState, Edge s) -> Maybe ATNState

q :: ATN s-> Set ATNState
q = undefined

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
atnOf g (ATNState{isS = s, isF = f, atnId = i}, NTE nte) = undefined

atnOf g (ATNState{isS = s, isF = f, atnId = i}, TE te  ) = undefined
atnOf g (ATNState{isS = s, isF = f, atnId = i}, PE pe  ) = undefined
atnOf g (ATNState{isS = s, isF = f, atnId = i}, ME me  ) = undefined
atnOf g (ATNState{isS = s, isF = f, atnId = i}, Epsilon) = undefined

