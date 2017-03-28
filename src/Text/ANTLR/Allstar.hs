{-# LANGUAGE ScopedTypeVariables #-}
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
data Parser s nt t = Parser
  -- Parser is the mutator state, i.e. current state of the parse
  { g   :: Grammar s nt t
  , i   :: Int
  , amb :: Map (ATNState nt) [Int]
  , s   :: s
  , dfa :: Set (DFAEdge nt t)
  , tokens :: [Lex.Token t]
  , stackSensitive :: Set (DFAState nt)
  , amb2 :: [([Lex.Token t], Lex.Token t, Set Int)]
  }

type ParserS s nt t a = State (Parser s nt t) a

data DFAEdge nt t = DFAE (DFAState nt) t (DFAState nt)
  deriving (Eq, Ord)

data DFAState nt  = ErrorState
                  | ConfState (Set (Configuration nt))
                  | FinalState Int -- production index
  deriving (Eq, Ord)
type Error = String
-- configuration
type Configuration nt = (ATNState nt, Int, Gamma nt)


-- data types

--depends on: adaptivePredict
--parse ::
parse 
  :: (NonTerminal nt, Terminal t, Ord nt, Ord t)
  => nt -> ParserS s nt t (Maybe ())
parse _S =
  let loop p _γ0 i =
        case p of
          Accept _B -> if _B == _S
                        then return $ Just () --success
                        else let ((q,_γ1):_) = pop _γ0
                             in  loop q _γ1 i
          _          ->
           do
             ATN {_Δ = delta} <-  getATN
             parser@(Parser{tokens = (curr_tok:rest), s=parser_state}) <- get
             -- should only ever be one by atn construction
             let [(_,t,q)] = Set.toList $ Set.filter (\(p',_,_) -> p' == p) delta
             case t of
               TE b -> if b == Lex.tok2term curr_tok
                                  then do
                                    put $ parser {tokens = rest}
                                    loop q _γ0 i
                                  else return Nothing
               NTE _B -> do
                 let _γ1 = push q _γ0
                 _i <- adaptivePredict _B _γ1
                 case _i of
                   Just i' -> loop (Middle _B i' 0) _γ1 i'
                   Nothing -> return Nothing
               PE (Predicate _π f) -> if f parser_state
                                      then loop q _γ0 i
                                      else return Nothing
               ME (Mutator _μ f) -> do
                put $ parser{s = f parser_state}
                loop q _γ0 i
               Epsilon -> loop q _γ0 i
  in do
       let gamma = Empty
       i <- adaptivePredict _S gamma

       case i of
         Just i -> let p = Middle _S i 0
                   in  loop p gamma i
         Nothing -> return Nothing


--depends on: llPredict
--            sllPredict
--            startState
--adaptivePredict ::
adaptivePredict
  :: forall s. forall nt. forall t. (NonTerminal nt, Terminal t, Ord nt, Ord t)
  => nt -> Gamma nt -> ParserS s nt t (Maybe Int)
adaptivePredict _A _γ0 =
  let hasPredForA :: [Production s nt t] -> Bool
      hasPredForA ((_,Sem _ _):_) = True
      hasPredForA [] = False
      hasPredForA (prod:prods) = hasPredForA prods

      --getD0 dfa = undefined
        --if dfa does not have start edge for _A
        --then do
          -- make it using startState
          -- update the DFA
        -- else
          -- grab the start state already in the DFA

  in do
      p@(Parser{tokens=ts,g=gram, dfa=_dfa}) <- get
      if hasPredForA $ prodsFor gram _A
        then do
          alt <- llPredict _A ts _γ0
          put $ p{tokens=ts} -- rewind input
          return alt
        else do
          -- dfa
          _D0 <- startState _A Wildcard
          alt <- sllPredict _A _D0 ts _γ0
          put $ p{tokens=ts}
          return alt




--depends on: closure
--startState ::
startState 
  :: (NonTerminal nt, Terminal t, Ord nt, Ord t)
  => nt -> Gamma nt -> ParserS s nt t (Set (Configuration nt))
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
sllPredict 
  :: forall nt. forall t. forall s. (NonTerminal nt, Ord nt, Terminal t, Ord t)
  => nt -> Set (Configuration nt) -> [Lex.Token t] -> Gamma nt -> ParserS s nt t (Maybe Int)
sllPredict _A d0 start _γ0 = do

  loop d0
    where
      findExistingTarget :: [DFAEdge nt t] -> DFAState nt -> t -> Maybe (DFAState nt)
      findExistingTarget ((DFAE d b d'):rest) d0 a
        | d == d0 && (sameTerminals a b) = Just d'
        | otherwise         = findExistingTarget rest d0 a
      findExistingTarget [] _ _ = Nothing
      loop :: Set (Configuration nt) -> ParserS s nt t (Maybe Int)
      loop d = do
        p@(Parser {tokens = (tok:rest), dfa = _dfa, stackSensitive=ss}) <- get
        let maybeD' = findExistingTarget (Set.toList _dfa) (ConfState d) (Lex.tok2term tok)
        _D' <- case maybeD' of
                 Just d' -> return d'
                 _       -> target d (Lex.tok2term tok)
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
target ::
  forall nt. forall t. forall s.
  (NonTerminal nt, Ord nt, Terminal t, Ord t)
  => Set (Configuration nt) -> t -> ParserS s nt t (DFAState nt)
target d0 a = do
  mv <- move d0 a
  ATN { _Δ = transitions } <- getATN
  let d' = Set.foldr (\c d -> d `Set.union` (closure transitions Set.empty c)) (Set.empty) mv
  checkConstraint d'
    where
      onlyOneProd confs =
        Set.size (Set.foldr (\(_,i,_) s -> Set.insert i s) Set.empty confs ) == 1
      checkConstraint :: Set (Configuration nt) -> ParserS s nt t (DFAState nt)
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
move ::
  forall nt. forall t. forall s. (NonTerminal nt, Terminal t, Ord nt, Ord t)
  => Set (Configuration nt) -> t -> ParserS s nt t (Set (Configuration nt))
move d a = do
  ATN {_Δ = delta} <- getATN
  return $ Set.foldr (fltr delta) Set.empty d
  where
    fltr :: Set (Transition s nt t) -> Configuration nt -> Set (Configuration nt) -> Set (Configuration nt)
    fltr delta (p0,i,gamma) d' = fromP (p0,i,gamma) (Set.toList delta) d'
    fromP :: Configuration nt -> [Transition s nt t] -> Set (Configuration nt) -> Set (Configuration nt)
    fromP _ [] d' = d'
    fromP (p0,i,gamma) ((p1,e,q1) : rest) d'
      | (e == TE a)  = fromP (p0,i,gamma) rest $ Set.insert (q1,i,gamma) d'
      | otherwise = fromP (p0,i,gamma) rest d'


--depends on: move
--            closure
--            getConflictSetsPerLoc
llPredict 
  :: (NonTerminal nt, Terminal t, Ord nt, Ord t)
  => nt -> [Lex.Token t] -> Gamma nt -> ParserS s nt t (Maybe Int)
llPredict _A start _γ0 =
  let
    loop _D = do
      p@(Parser {tokens = (cur:rest), amb2 = a2}) <- get
      ATN {_Δ = delta} <- getATN
      let term = Lex.tok2term cur
      mv <- move _D term
      let _D' = Set.foldr (Set.union . (closure delta Set.empty)) Set.empty mv
      case (Set.toList . getJs) _D' of
        []  -> return Nothing
        [i] -> return $ Just i
        _   -> do -- ambiguous case
           let altsets = getConflictSetsPerLoc _D'
           if (Set.size altsets == 1) && (Set.size (Set.elemAt 0 altsets) > 1)
             then do
               let x = (Set.elemAt 0 altsets)
               put $ p{amb2 = (start, cur, x) : a2}
               return $ Just $ Set.elemAt 0 x
             else do
               put $ p {tokens = rest}
               loop _D'



    getJs confs = Set.foldr (\(_,i,_) s -> Set.insert i s) Set.empty confs
  in
    do
      _D <- startState _A _γ0
      loop _D


--getATN = return $ ATN { _Δ = Set.empty }
getATN :: (NonTerminal nt, Terminal t, Ord nt, Ord t) => ParserS s nt t (ATN s nt t)
getATN = do
  Parser { g = grammar} <- get
  return $ atnOf grammar


--no fn dependencies
closure ::
  forall nt. forall t. forall s. (NonTerminal nt, Terminal t, Ord nt, Ord t)
  => Set (Transition s nt t) -> Set (Configuration nt) -> Configuration nt -> Set (Configuration nt)
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
    cfgsAfterNTEdge :: [Transition s nt t] -> [Configuration nt]
    cfgsAfterNTEdge [] = []
    cfgsAfterNTEdge ((p0, (NTE nt), q0):rest) = (q0,i,Wildcard) : cfgsAfterNTEdge rest
    cfgsAfterNTEdge (_:rest) = cfgsAfterNTEdge rest
    cfgsNonEmpty :: [Configuration nt]
    cfgsNonEmpty =
      let toConf :: (ATNState nt, Gamma nt) -> Configuration nt
          toConf (q,g') = (q,i, g')
      in map toConf (pop gamma)
    cfgsNonEnd :: [Transition s nt t] -> ATNState nt -> [Configuration nt]
    cfgsNonEnd [] _ = []
    cfgsNonEnd ((p0, e, q0):rest) p
      | p0 == p   = case e of
                      TE _    -> (cfgsNonEnd rest p)
                      NTE nt  -> (Start nt, i, push q0 gamma) : (cfgsNonEnd rest p)
                      _       -> (q0,i,gamma) : (cfgsNonEnd rest p)
      | otherwise = (cfgsNonEnd rest p)
    cfgSubRoutine :: [Transition s nt t] -> Set (Configuration nt) -> Set (Configuration nt)
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
getConflictSetsPerLoc :: forall nt. (NonTerminal nt, Ord nt) => Set (Configuration nt) -> Set (Set Int)
getConflictSetsPerLoc d =
  let m = Set.foldr updateEntry (Map.empty) d
      updateEntry :: Configuration nt -> (Map (ATNState nt, Gamma nt) (Set Int)) -> (Map (ATNState nt, Gamma nt) (Set Int))
      updateEntry (p,i,g) i_map = Map.alter (updateSet i) (p,g) i_map
      updateSet :: Int -> Maybe (Set Int) -> Maybe (Set Int)
      updateSet i Nothing           = Just $ Set.singleton i
      updateSet i (Just i_set)      = Just $ Set.insert i i_set
  in  Set.fromList (Map.elems m)



-- no dependencies
-- for each p return set of alts i from (p,-,-) in D Confs
--getProdSetsPerState ::
getProdSetsPerState :: forall nt. (NonTerminal nt, Ord nt) => Set (Configuration nt) -> Set (Set Int)
getProdSetsPerState d =
  let m = Set.foldr updateEntry (Map.empty) d
      updateEntry :: Configuration nt -> (Map (ATNState nt) (Set Int)) -> (Map (ATNState nt) (Set Int))
      updateEntry (p,i,_) i_map = Map.alter (updateSet i) p i_map
      updateSet :: Int -> Maybe (Set Int) -> Maybe (Set Int)
      updateSet i Nothing           = Just $ Set.singleton i
      updateSet i (Just i_set)      = Just $ Set.insert i i_set
  in  Set.fromList (Map.elems m)
