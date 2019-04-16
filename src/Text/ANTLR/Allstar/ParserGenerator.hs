{-# LANGUAGE TypeFamilies, FlexibleContexts, BangPatterns #-}
{-|
  Module      : Text.ANTLR.Allstar.ParserGenerator
  Description : ALL(*) parsing algorithm
  Copyright   : (c) Sam Lasser,
                (c) Karl Cronburg, 2017-2018
  License     : BSD3
  Maintainer  : karl@cs.tufts.edu
  Stability   : experimental
  Portability : POSIX

-}
module Text.ANTLR.Allstar.ParserGenerator
  ( GrammarSymbol(..), ATNEnv(..)
  , AST(..), ATNState(..), ATNEdge(..)
  , ATNEdgeLabel(..)
  , parse
  ) where

import Data.List
import qualified Data.Set as DS
import Debug.Trace as D
import Data.Maybe (fromJust)

import Text.ANTLR.Parser (HasEOF(..))
import Text.ANTLR.Grammar (Ref(..))

--------------------------------TYPE DEFINITIONS--------------------------------

-- Add another synonym for NT names
-- Change ATN repr. so NT name is the key, NT name doesn't appear in state identifiers
-- Consider more nested representation of ATN, where integer path IDs are keys

-- | Grammar symbol types
data GrammarSymbol nt t = NT nt | T t | EPS deriving (Eq, Ord, Show)

-- ATN types

{-

type ATN nt t       = [ATNPath nt t]
type ATNPath nt t   = [ATNEdge nt t]
type ATNEdge nt t   = (ATNState nt, ATNEdgeLabel nt t, ATNState nt)
data ATNEdgeLabel nt t = GS (GrammarSymbol nt t) | PRED Bool
data ATNState nt    = INIT nt | CHOICE nt Int | MIDDLE Int | FINAL nt deriving
                      (Eq, Ord, Show)

-}

-- | Specifies the nonterminal we're currently parsing as well as
--   what state we are in for parsing some NT symbol.
data ATNState nt =
    Init nt            -- ^ Starting state
  | Middle nt Int Int  -- ^ Intermediate state
  | Final nt           -- ^ Accepting state
  deriving (Eq, Ord, Show)

-- | Starting state, NT/T symbol to parse, and ending state.
type ATNEdge nt t = (ATNState nt, ATNEdgeLabel nt t, ATNState nt)

-- | The domain of labels on edges in an augmented recursive transition network,
--   namely the symbol we parse upon traversing an edge.
data ATNEdgeLabel nt t =
    GS (GrammarSymbol nt t)  -- ^ The symbol to parse upon traversing an edge
  | PRED Bool                -- ^ Unimplemented predicates in ALL(*)
  deriving (Eq, Ord, Show)

-- | A set of ATN edges, defining the grammar over which the ALL(*) parsing
--   algorithm operates.
type ATNEnv nt t = DS.Set (ATNEdge nt t)

isInit :: ATNState nt -> Bool
isInit (Init nt) = True
isInit _ = False

outgoingEdge :: (Eq nt, Show nt) => ATNState nt -> ATNEnv nt t -> ATNEdge nt t
outgoingEdge p atnEnv = let edges = outgoingEdges p atnEnv
                        in  case edges of
                              [edge] -> edge
                              _ -> error "Multiple edges found"

-- Do I need to make sure there's at least one outgoing edge, or should that
-- be handled by an earlier check to make sure the grammar/ATN is well-formed?
outgoingEdges :: (Eq nt, Show nt) => ATNState nt -> ATNEnv nt t -> [ATNEdge nt t]
outgoingEdges p atnEnv = DS.toList (DS.filter (\(p',_,_) -> p' == p) atnEnv)



type ATNStack nt    = [ATNState nt]
-- type ATNEnv nt t    = [(GrammarSymbol nt t, ATN nt t)]
type ATNConfig nt   = (ATNState nt, Int, ATNStack nt)

-- DFA types
type DFA nt t       = [DFAEdge nt t]
type DFAEdge nt t   = (DFAState nt, t, DFAState nt)
data DFAState nt    = Dinit [ATNConfig nt] | D [ATNConfig nt] | F Int | Derror deriving (Eq, Ord, Show)
type DFAEnv nt t    = [(GrammarSymbol nt t, DFA nt t)]

getLabel :: (Ref v, HasEOF (Sym v)) => v -> StripEOF (Sym v)
getLabel = fromJust . stripEOF . getSymbol

type Label tok = StripEOF (Sym tok)

-- | Input sequence type
{-class Token t where
  type Label t :: *
  type Literal t :: *
  getLabel   :: t -> Label t
  getLiteral :: t -> Literal t
-}

-- | Return type of parse function
data AST nt tok = Node nt [AST nt tok] | Leaf tok deriving (Eq, Show)

--------------------------------CONSTANTS---------------------------------------

emptyEnv        = []
emptyStack      = []
emptyDerivation = []

--------------------------------AUXILIARY FUNCTIONS-----------------------------

{-

-- Return the ATN edge with ATN state p on the left
-- Q: Should I handle the "no edge found" case here, or in the caller?
outgoingEdge :: Eq nt => ATNState nt -> ATNEnv nt t -> Maybe (ATNEdge nt t)
outgoingEdge p atnEnv = let edges = (concat . concat) (map snd atnEnv)
                        in  find (\(p', t, q) -> p == p') edges

-- Return all ATN edges with ATN state p on the left
outgoingEdges :: Eq nt => ATNState nt -> ATNEnv nt t -> [ATNEdge nt t]
outgoingEdges p atnEnv = let edges = (concat . concat) (map snd atnEnv)
                         in  filter (\(p', t, q) -> p == p') edges

-}

-- Better way to ensure that the parameter is a D DFA state?
getConflictSetsPerLoc :: (Eq nt, Ord nt) => DFAState nt -> [[ATNConfig nt]]
getConflictSetsPerLoc q =
  case q of
    F _       -> error "final state passed to getConflictSetsPerLoc"
    Derror    -> error "error state passed to getConflictSetsPerLoc"
    D configs -> let sortedConfigs = sortOn (\(p, i, gamma) -> (p, gamma)) configs
                 in  groupBy (\(p, i, gamma) (p', j, gamma') ->
                               p == p' && i /= j && gamma == gamma')
                             sortedConfigs

getProdSetsPerState :: (Eq nt, Ord nt) => DFAState nt -> [[ATNConfig nt]]
getProdSetsPerState q = 
  case q of
    F _       -> error "final state passed to getProdSetsPerState"
    Derror    -> error "error state passed to getProdSetsPerState"
    D configs -> let sortedConfigs = sortOn (\(p, i, gamma) -> (p, gamma)) configs
                 in  groupBy (\(p, _, _) (p', _, _) -> p == p')
                             sortedConfigs

dfaTrans :: (Eq nt, Eq t) => DFAState nt -> t -> DFA nt t -> Maybe (DFAEdge nt t)
dfaTrans d t dfa = find (\(d1, label, _) -> d1 == d && label == t) dfa

findInitialState :: DFA nt t -> Maybe (DFAState nt)
findInitialState dfa =
  let isInit d = case d of
                   Dinit _ -> True
                   _       -> False
  in  case find (\(d1, _, _) -> isInit d1) dfa of
        Just (d1, _, _) -> Just d1
        Nothing         -> Nothing


allEqual :: Eq a => [a] -> Bool
allEqual []       = True
allEqual (x : xs) = all (== x) xs

bind :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
bind k v []             = [(k, v)]
bind k v ((k', v') : al') = if k == k' then (k, v) : al' else (k', v') : bind k v al'

--------------------------------ALL(*) FUNCTIONS--------------------------------
-- should parse() also return residual input sequence?

-- | ALL(*) parsing algorithm. This is __not__ the entrypoint as used by
--   user-facing code. See 'Text.ANTLR.Allstar.parse' instead.
parse :: (Eq nt, Show nt, Ord nt, Eq (Label tok), Show (Label tok), Ord (Label tok), Show tok
         , Ref tok, HasEOF (Sym tok)) =>
         [tok] -> GrammarSymbol nt (Label tok) -> ATNEnv nt (Label tok) -> Bool -> Either String (AST nt tok)
parse input startSym atnEnv useCache =
  let parseLoop input currState stack dfaEnv subtrees astStack =
        case (currState, startSym) of
          (Final c, NT c') ->
            if c == c' then
              Right (Node c subtrees)
            else
              case (stack, astStack) of
                (q : stack', leftSiblings : astStack') ->
                  parseLoop input q stack' dfaEnv (leftSiblings ++ [Node c subtrees]) astStack'
                _ -> error ("Reached a final ATN state, but parse is incomplete " ++
                            "and there's no ATN state to return to")
          (_, _) ->
            case (outgoingEdge currState atnEnv) of
              -- Nothing -> error ("No matching edge found for " ++ (show currState))
              (p, t, q) ->
                case (t, input) of
                  (GS (T b), [])     -> error "Input has been exhausted"
                  (GS (T b), c : cs) -> if b == getLabel c then
                                          parseLoop cs q stack dfaEnv (subtrees ++ [Leaf c]) astStack -- changed from Leaf b
                                        else
                                          Left ("remaining input: " ++ show input)
                  (GS (NT b), _)     -> let stack'       = q : stack
                                        in  case adaptivePredict (NT b) input stack' dfaEnv of  -- Pattern for referring to (NT b)?
                                              Nothing -> Left ("Couldn't find a path through ATN " ++ show b ++
                                                               " with input " ++ show input)
                                              Just (i, dfaEnv') -> parseLoop input (Middle b i 0) stack' dfaEnv' [] (subtrees : astStack) -- was (CHOICE b i)
                  (GS EPS, _)        -> parseLoop input q stack dfaEnv subtrees astStack
                  (PRED _, _)        -> error "not implemented"

      initialDfaEnv = DS.toList (DS.foldr (\(p,_,_) ntNames ->
                                  case p of Init ntName -> DS.insert (NT ntName, []) ntNames
                                            _ -> ntNames)
                                DS.empty
                                atnEnv)
      
  in  case startSym of (NT c) ->
                         case adaptivePredict startSym input emptyStack initialDfaEnv of
                           Nothing -> Left ("Couldn't find a path through ATN " ++ show c ++
                                            " with input " ++ show input)
                           Just (iStart, initialDfaEnv') -> parseLoop input (Middle c iStart 0) emptyStack initialDfaEnv' [] emptyStack
                       _ -> error "Start symbol must be a nonterminal"

  where

    -- adaptivePredict :: (GrammarSymbol nt (Label tok)) -> [tok] -> ATNStack nt -> DFAEnv nt (Label tok) -> Maybe (Int, DFAEnv nt (Label tok))
    adaptivePredict sym input stack dfaEnv  =
      case lookup sym dfaEnv of
        Nothing  -> error ("No DFA found for " ++ show sym)
        Just dfa -> let d0  = case findInitialState dfa of
                                Just d0 -> d0
                                Nothing -> startState sym emptyStack
                        in sllPredict sym input d0 stack dfaEnv

    startState sym stack =
      case sym of
        NT ntName ->
          let initEdges = outgoingEdges (Init ntName) atnEnv
              loopOverAtnPaths initEdges =
                case initEdges of
                  [] -> []
                  (Init _, GS EPS, q@(Middle _ i _)) : es ->
                    (closure [] (q, i, stack)) ++ loopOverAtnPaths es
                  _ -> error "ATN path must begin with an epsilon edge from Init to Choice"
          in  D (loopOverAtnPaths initEdges)
        _ -> error "Symbol passed to startState must be a nonterminal"

    closure busy currConfig =
      if elem currConfig busy then
        []
      else
        let busy'            = currConfig : busy
            (p, i, gamma)    = currConfig
            pEdges           = outgoingEdges p atnEnv
            loopOverEdges es =
              case es of
                []                      -> [] 
                (_, GS (NT ntName), q) : es' ->
                  closure busy' (Init ntName, i, q : gamma) ++
                  loopOverEdges es'
                (_, GS EPS, q) : es'       ->
                  closure busy' (q, i, gamma) ++
                  loopOverEdges es'
                (_, GS (T _), _) : es'       ->
                  loopOverEdges es'
        in  case (p, gamma) of
          (Final _, [])         -> [currConfig]
          (Final _, q : gamma') -> currConfig : closure busy' (q, i, gamma')
          _                     -> currConfig : loopOverEdges pEdges

    sllPredict sym input d0 stack initialDfaEnv =
      let predictionLoop d tokens dfaEnv =
            case tokens of
              []     -> Nothing -- Does the empty token sequence ever indicate that the grammar is ambiguous?
              t : ts ->
                let (d', dfaEnv') =
                      if useCache then
                        case lookup sym dfaEnv of
                          Nothing  -> error ("No DFA found for nonterminal " ++ show sym ++ show dfaEnv)
                          Just dfa ->
                            case dfaTrans d (getLabel t) dfa of
                              Just (_, _, d2) -> (d2, dfaEnv)
                              Nothing         -> let d' = target d t
                                in  (d', bind sym ((d, getLabel t, d') : dfa) dfaEnv)
                      else
                        (target d t, dfaEnv) -- don't use the cache, or add any new information to it
                in  case d' of
                      Derror            -> Nothing
                      F i               -> Just (i, dfaEnv')
                      D atnConfigs      ->
                        let conflictSets   = getConflictSetsPerLoc d'
                            prodSets       = getProdSetsPerState d'
                            stackSensitive =
                              any (\cSet -> length cSet > 1) conflictSets &&
                              not (any (\pSet -> length pSet == 1) prodSets)
                        in  if stackSensitive then
                              Just (llPredict sym input stack, initialDfaEnv) -- Again, do we have to discard previous updates to the DFA?
                            else
                              predictionLoop d' ts dfaEnv'
      in  predictionLoop d0 input initialDfaEnv

    -- This function looks a little fishy -- come back to it and think about what each case represents
    -- Also, maybe it should return a Maybe type so that it can propagate a Nothing value upwards
    -- instead of raising an error
    llPredict sym input stack =
      let d0 = startState sym stack
          predictionLoop d tokens =
            case tokens of
              []     -> error ("Empty input in llPredict")
              t : ts -> 
                let mv = move d (getLabel t)
                    d' = D (concat (map (closure []) mv))
                in  case d' of
                      D []         -> error ("empty DFA state in llPredict")
                      D atnConfigs ->
                        case nub (map (\(_, j, _) -> j) atnConfigs) of
                          [i] -> i
                          _   ->
                            let altSets = getConflictSetsPerLoc d'
                            in  case altSets of
                                  []     -> error ("No alt sets found")
                                  a : as ->
                                    if allEqual altSets && length a > 1 then
                                      minimum (map (\(_, i, _) -> i) a)
                                    else
                                      predictionLoop d' ts
      in  predictionLoop d0 input
      

    target d a =
      let mv = move d (getLabel a)
          d' = D (concat (map (closure []) mv))
      in  case d' of
            D []         -> Derror
            D atnConfigs ->
              case nub (map (\(_, j, _) -> j) atnConfigs) of
                [i] -> F i
                _   -> d'
                
    move q t = 
      case q of
        D atnConfigs ->
          let qsForP (p, i, gamma) =
                let pOutgoingEdges = outgoingEdges p atnEnv
                in  foldr (\(p', label, q) acc ->
                            case label of
                              GS (T a) -> if t == a then
                                            (q, i, gamma) : acc
                                          else
                                            acc
                              _   -> acc)
                          []
                          pOutgoingEdges
          in concat (map qsForP atnConfigs)

