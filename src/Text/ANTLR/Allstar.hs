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
import Data.Map (Map)
import qualified Data.Map as Map

-- parser
data Parser s = Parser
  -- Parser is the mutator state, i.e. current state of the parse
  { g   :: Grammar s
  , i   :: Int
  , amb :: Map ATNState [Int]
  , s   :: s
  }

type ParserS s a = State (Parser s) a

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
startState :: NonTerminal -> Gamma -> ParserS s (Set Configuration)
startState a gamma = do
  ATN {_Δ = delta} <-  getATN
  Parser {s = parser_state} <- get
  return $ Set.foldr (getProdStarts delta parser_state) Set.empty delta
    where
      getProdStarts delta parser_state (Start a, Epsilon, Middle _ i 0) confs =
        confs `Set.union` closure delta Set.empty (Start a,i,Empty)
      getProdStarts delta parser_state (Start a, PE (Predicate _ fn), Middle _ i 0) confs
        | fn parser_state = confs `Set.union` closure delta Set.empty (Start a,i,Empty)
        | otherwise = confs
      getProdStarts delta parser_state _ confs = confs

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
move :: Set Configuration -> Terminal -> ParserS s (Set Configuration)
move d a = do
  ATN {_Δ = delta} <- getATN
  return $ Set.foldr (fltr delta) Set.empty d
  where
    fltr :: Set (Transition s) -> Configuration -> Set Configuration -> Set Configuration
    fltr delta (p0,i,gamma) d' = fromP (p0,i,gamma) (Set.toList delta) d'
    fromP :: Configuration -> [Transition s] -> Set Configuration -> Set Configuration
    fromP _ [] d' = d'
    fromP (p0,i,gamma) ((p1,e,q1) : rest) d'
      | (e == TE a)  = fromP (p0,i,gamma) rest $ Set.insert (q1,i,gamma) d'
      | otherwise = fromP (p0,i,gamma) rest d'


--depends on: move
--            closure
--            getConflictSetsPerLoc
llPredict :: NonTerminal -> ATNState -> Gamma -> ParserS s (Maybe Int)
llPredict = undefined

--getATN = return $ ATN { _Δ = Set.empty }
getATN :: ParserS s (ATN s)
getATN = do
  Parser { g = grammar} <- get
  return $ atnOf grammar


--no fn dependencies
closure :: Set (Transition s) -> Set Configuration -> Configuration -> Set Configuration
closure d busy (cfg@(p,i,gamma))
  | Set.member cfg busy = Set.empty
  | otherwise =
      let busy' = busy `Set.union` Set.singleton cfg
          edges    = (Set.toList d)
          cfg_set  = Set.singleton cfg
      in  cfgSubRoutine edges cfg_set
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
    cfgSubRoutine :: [Transition s] -> Set Configuration -> Set Configuration
    cfgSubRoutine edges cfg_set=
      case p of
        Accept nt -> (
          if isWildcard gamma
          then
            let cfgs = (map (closure d busy) (cfgsAfterNTEdge edges))
            in  foldr Set.union cfg_set cfgs
          else
            let cfgs = (map (closure d busy) (cfgsNonEmpty))
            in  foldr Set.union cfg_set cfgs
          )
        p'        ->
          let cfgs = (map (closure d busy) (cfgsNonEnd edges p'))
          in  foldr Set.union cfg_set cfgs

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
getConflictSetsPerLoc :: Set Configuration -> Set (Set Int)
getConflictSetsPerLoc d =
  let m = Set.foldr updateEntry (Map.empty) d
      updateEntry :: Configuration -> (Map (ATNState,Gamma) (Set Int)) -> (Map (ATNState,Gamma) (Set Int))
      updateEntry (p,i,g) i_map = Map.alter (updateSet i) (p,g) i_map
      updateSet :: Int -> Maybe (Set Int) -> Maybe (Set Int)
      updateSet i Nothing           = Just $ Set.singleton i
      updateSet i (Just i_set)      = Just $ Set.insert i i_set
  in  Set.fromList (Map.elems m)



-- no dependencies
-- for each p return set of alts i from (p,-,-) in D Confs
--getProdSetsPerState ::
getProdSetsPerState :: Set Configuration -> Set (Set Int)
getProdSetsPerState d =
  let m = Set.foldr updateEntry (Map.empty) d
      updateEntry :: Configuration -> (Map ATNState (Set Int)) -> (Map (ATNState) (Set Int))
      updateEntry (p,i,_) i_map = Map.alter (updateSet i) p i_map
      updateSet :: Int -> Maybe (Set Int) -> Maybe (Set Int)
      updateSet i Nothing           = Just $ Set.singleton i
      updateSet i (Just i_set)      = Just $ Set.insert i i_set
  in  Set.fromList (Map.elems m)
