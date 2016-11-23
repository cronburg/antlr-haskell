module Text.Allstar.Types where
import qualified Data.Set as Set
import Data.Set (Set(..))
import Prelude hiding (pi)
import Control.Monad.State
-- parser
data Parser = Parser { g :: Grammar

                     }
type ParserS a = State Parser a

-- grammar
type NonTerminal = String
type Terminal    = String
data Production  = Prod {pLHS :: NonTerminal, pRHS :: [Either NonTerminal Terminal]}
type Predicate   = Parser -> Bool
type Mutator     = Parser -> Parser

data Grammar = G { n  :: Set NonTerminal
                 , t  :: Set Terminal
                 , p  :: [Production]
                 , s  :: NonTerminal
                 , pi :: [Predicate]
                 , m  :: [Mutator]
                 }
-- Graph Structured Stack
type Gamma = GSS ATNState

data GSS a = GSSEmpty
           | GSSWildcard
           | GSSStack [a]
           | GSSMerge (GSS a) (GSS a)

(\+/) = GSSMerge
(#) = GSSWildcard

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


-- configuration
type Configuration = (ATNState, Int, Gamma)
