module Text.Allstar where
import Text.Allstar.Grammar
import Text.Allstar.GSS
import Text.Allstar.ATN

-- Set
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State

-- parser
data Parser = Parser
  -- Parser is the mutator state, i.e. current state of the parse
  { g :: Grammar Parser
  }

type ParserS a = State Parser a

-- configuration
type Configuration = (ATNState, Int, Gamma)

-- data types

--depends on: adaptivePredict
--parse ::
parse = undefined


--depends on: llPredict
--            sllPredict
--            startState
--adaptivePredict ::
adaptivePredict = undefined


--depends on: closure
--startState ::
startState = undefined



--depends on: target
--            llPredict
--sllPredict ::
sllPredict = undefined

--depends on: move
--            closure
--            getConflictSetsPerLoc
--            getProdSetsPerState
--target ::
target = undefined
-- no dependencies
-- set of all (q,i,Gamma) s.t. p -a> q and (p,i,Gamma) in State d
--move ::
move = undefined


--depends on: move
--            closure
--            getConflictSetsPerLoc
llPredict = undefined

--getATN = return $ ATN { _Δ = Set.empty }

--no fn dependencies
closure :: Set Configuration -> Configuration -> ParserS (Set Configuration)
closure cfgs cfg = undefined {-do
  _Δ <- getATN
  let
    closure' (Accept pb', i, Wildcard) =
      foldr merge [ closure busy (p2, i, Wildcard)
                  | p2 <- fL _Δ
                  , (NTE nt, p2) `member` map (\(_,e,s) -> (e,s)) _Δ
                  ]
-}

-- TODO: first class patterns to functions, e.g. member takes in the pattern
-- over the tuple / type of thing that is in the set.
--                  , Pattern (p1, Captured (NTE ^nt), p2) `member` set

-- no dependencies
-- for each p,Gamma: get set of alts {i} from (p,-,Gamma) in D Confs
--getConflictSetsPerLoc ::
getConflictSetsPerLoc = undefined

-- no dependencies
-- for each p return set of alts i from (p,-,-) in D Confs
--getProdSetsPerState ::
getProdSetsPerState = undefined

