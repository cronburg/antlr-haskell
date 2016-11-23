module Text.Allstar.ATN where
-- Augmented Transition Network
import Text.Allstar.Grammar

-- ATN State
data ATNState = ATNState {isS :: Bool, isF :: Bool, atnId :: Int}
type ATN = (ATNState,Edge) -> Maybe ATNState

q :: ATN -> Set ATNState
q = undefined

sigma :: ATN -> [Edge]
sigma = undefined

e :: ATN -> Set ATNState
e = undefined

f :: ATN -> Set ATNState
f = undefined

data Edge = NTE NonTerminal
          | TE  Terminal
          | PE  Predicate
          | ME  Mutator
          | Epsilon
-- atnOf :: Grammar -> (ATNState,Edge) -> Maybe ATNState
atnOf :: Grammar -> ATN
atnOf g (ATNState{isS = s, isF = f, atnId = i}, NTE nte) = undefined

atnOf g (ATNState{isS = s, isF = f, atnId = i}, TE te  ) = undefined
atnOf g (ATNState{isS = s, isF = f, atnId = i}, PE pe  ) = undefined
atnOf g (ATNState{isS = s, isF = f, atnId = i}, ME me  ) = undefined
atnOf g (ATNState{isS = s, isF = f, atnId = i}, Epsilon) = undefined

