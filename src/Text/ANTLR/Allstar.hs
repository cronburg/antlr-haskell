module Text.ANTLR.Allstar where
import Text.ANTLR.Allstar.Grammar
--import Text.ANTLR.Allstar.GSS
import Text.ANTLR.Allstar.ATN
import Text.ANTLR.Allstar.Stacks
import qualified Text.ANTLR.Allstar.Lex as Lex
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
  , dfa :: Set DFAEdge
  , tokens :: [Lex.Token]
  , stackSensitive :: Set DFAState
  }

type ParserS s a = State (Parser s) a

data DFAEdge = DFAE DFAState Terminal DFAState
  deriving (Eq, Ord)
data DFAState = ErrorState
              | ConfState (Set Configuration)
              | FinalState Int -- production index
  deriving (Eq, Ord)
type Error = String
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
sllPredict :: NonTerminal -> Set Configuration -> [Lex.Token] -> Gamma -> ParserS s (Maybe Int)
sllPredict _A d0 start _γ0 = do

  loop d0
    where
      findExistingTarget :: [DFAEdge] -> DFAState -> Terminal -> Maybe DFAState
      findExistingTarget ((DFAE d b d'):rest) d0 a
        | d == d0 && a == b = Just d'
        | otherwise         = findExistingTarget rest d0 a
      findExistingTarget [] _ _ = Nothing
      loop :: Set Configuration -> ParserS s (Maybe Int)
      loop d = do
        p@(Parser {tokens = (a:rest), dfa = _dfa, stackSensitive=ss}) <- get
        let maybeD' = findExistingTarget (Set.toList _dfa) (ConfState d) a
        _D' <- case maybeD' of
                 Just d' -> return d'
                 _       -> target d (Lex.termOf a)
        case (_D', Set.member _D' ss)  of
          (_, True) -> do
            put $ p {tokens = start}
            llPredict _A start _γ0
          (FinalState i,_) -> return $ Just i
          (ConfState _C,_) -> do
            put $ p {tokens = rest}
            loop _C
          _ -> return Nothing






--depends on: move
--            closure
--            getConflictSetsPerLoc
--            getProdSetsPerState
--target ::
target :: Set Configuration -> Terminal -> ParserS s (DFAState)
target d0 a = do
  mv <- move d0 a
  ATN { _Δ = transitions } <- getATN
  let d' = Set.foldr (\c d -> d `Set.union` (closure transitions Set.empty c)) (Set.empty) mv
  checkConstraint d'
    where
      onlyOneProd confs =
        Set.size (Set.foldr (\(_,i,_) s -> Set.insert i s) Set.empty confs ) == 1
      checkConstraint :: Set Configuration -> ParserS s DFAState
      checkConstraint d'
        | Set.null d' = do
            p@ (Parser {dfa=delta}) <- get
            put $ p {dfa=Set.insert (DFAE (ConfState d0) a (ErrorState)) delta}
            return ErrorState
        | onlyOneProd d' = do
            p@(Parser {dfa=delta}) <- get
            let ((_,i,_):_) = Set.toList d'
            put $ p {dfa=Set.insert (DFAE (ConfState d0) a (FinalState i)) delta}
            return $ FinalState i
        | otherwise = do
            let a_conflict = not $ Set.null $ Set.filter (\is -> (Set.size is) > 1) (getConflictSetsPerLoc d')
            let viable = not $ Set.null $ Set.filter (\is -> (Set.size is) == 1) (getProdSetsPerState d')
            p@(Parser{stackSensitive=ss, dfa=delta}) <- get
            let ss' = if a_conflict && (not viable)
                        then Set.insert (ConfState d') ss
                        else ss
            put $ p {stackSensitive=ss',dfa = Set.insert (DFAE (ConfState d0) a (ConfState d')) delta}
            return $ ConfState d'


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
llPredict :: NonTerminal -> [Lex.Token] -> Gamma -> ParserS s (Maybe Int)
llPredict a start g0 = undefined
  -- let
  --    loop :: Set Configuration -> ParserS s (Maybe Int)
  --    loop d = do
  --      Parser {i = i', tokens = ts} <- get
  --      let curr = ts !! i'
  --      mv <- move d curr
  --      let d' = Set.foldr (Set.union . (closure Set.empty)) Set.empty mv
  --      -- TODO
  -- in do
  --      d <- startState a g0
  --      loop d


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
