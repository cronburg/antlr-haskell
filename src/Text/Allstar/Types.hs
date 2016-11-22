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
sigma :: ATN -> [Edge]
e :: ATN -> Set ATNState
f :: ATN -> Set ATNState

data Edge = NTE NonTerminal
          | TE  Terminal
          | PE  Predicate
          | ME  Mutator
          | Epsilon
-- atnOf :: Grammar -> (ATNState,Edge) -> Maybe ATNState
atnOf :: Grammar -> ATN
atnOf g (ATNState{isS = s, isF = f, atnId = i}, NTE nte) =

atnOf g (ATNState{isS = s, isF = f, atnId = i}, TE te  ) =
atnOf g (ATNState{isS = s, isF = f, atnId = i}, PE pe  ) =
atnOf g (ATNState{isS = s, isF = f, atnId = i}, ME me  ) =
atnOf g (ATNState{isS = s, isF = f, atnId = isF = True i, _} = , Epsilon) =


-- configuration
type Configuration = (ATNState, Int, Gamma)
