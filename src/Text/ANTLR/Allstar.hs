module Text.ANTLR.Allstar where
import Text.ANTLR.Allstar.Grammar
--import Text.ANTLR.Allstar.GSS
import Text.ANTLR.Allstar.ATN
import Text.ANTLR.Allstar.Stacks
-- Set
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad (mapM)

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
move :: Set Configuration -> Terminal -> ParserS (Set Configuration)
move d a = do
  ATN {_Δ = delta} <- getATN
  return $ Set.foldr (fltr delta) Set.empty d
  where
    fltr :: Set (Transition Parser) -> Configuration -> Set Configuration -> Set Configuration
    fltr delta (p0,i,gamma) d' = fromP (p0,i,gamma) (Set.toList delta) d'
    fromP :: Configuration -> [Transition Parser] -> Set Configuration -> Set Configuration
    fromP _ [] d' = d'
    fromP (p0,i,gamma) ((p1,e,q1) : rest) d'
      | (e == TE a)  = fromP (p0,i,gamma) rest $ Set.insert (q1,i,gamma) d'
      | otherwise = fromP (p0,i,gamma) rest d'


--depends on: move
--            closure
--            getConflictSetsPerLoc
llPredict = undefined

--getATN = return $ ATN { _Δ = Set.empty }
getATN :: ParserS (ATN Parser)
getATN = do
  Parser { g = grammar} <- get
  return $ atnOf grammar


--no fn dependencies
closure :: Set Configuration -> Configuration -> ParserS (Set Configuration)
closure busy (cfg@(p,i,gamma))
  | Set.member cfg busy = return Set.empty
  | otherwise = do
      let busy' = busy `Set.union` Set.singleton cfg
      ATN {_Δ = d} <- getATN
      let edges    = (Set.toList d)
      let cfg_set  = Set.singleton cfg
      cfgSubRoutine edges cfg_set
  where
    isWildcard :: Stacks a -> Bool
    isWildcard Wildcard = True
    isWildcard _        = False
    cfgsAfterNTEdge :: [Transition s] -> [Configuration]
    cfgsAfterNTEdge [] = []
    cfgsAfterNTEdge ((p0, (NTE nt), q0):rest) = (q0,i,Wildcard) : cfgsAfterNTEdge rest
    cfgsAfterNTEdge (_:rest) = cfgsAfterNTEdge rest
    cfgsNonEmpty :: [Configuration]
    cfgsNonEmpty =
      let toConf :: (ATNState, Gamma) -> Configuration
          toConf (q,g') = (q,i, g')
      in map toConf (pop gamma)
    cfgsNonEnd :: [Transition s] -> ATNState -> [Configuration]
    cfgsNonEnd [] _ = []
    cfgsNonEnd ((p0, e, q0):rest) p
      | p0 == p   = case e of
                      TE _    -> (cfgsNonEnd rest p)
                      NTE nt  -> (Start nt, i, push q0 gamma) : (cfgsNonEnd rest p)
                      _       -> (q0,i,gamma) : (cfgsNonEnd rest p)
      | otherwise = (cfgsNonEnd rest p)
    cfgSubRoutine :: [Transition s] -> Set Configuration -> ParserS (Set Configuration)
    cfgSubRoutine edges cfg_set=
      case p of
        Accept nt -> (
          if isWildcard gamma
          then do
            cfgs <- (mapM (closure busy) (cfgsAfterNTEdge edges))
            return $ foldr Set.union cfg_set cfgs
          else do
            cfgs <- (mapM (closure busy) (cfgsNonEmpty))
            return $ foldr Set.union cfg_set cfgs
          )
        p'        -> do
          cfgs <- (mapM (closure busy) (cfgsNonEnd edges p'))
          return $ foldr Set.union cfg_set cfgs

 {-do
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
