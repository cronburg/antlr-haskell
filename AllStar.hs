module ParserGenerator.AllStar where

import Data.List
import Test.HUnit
import Debug.Trace

--------------------------------TYPE DEFINITIONS--------------------------------

-- Add another synonym for NT names
-- Change ATN repr. so NT name is the key, NT name doesn't appear in state identifiers
-- Consider more nested representation of ATN, where integer path IDs are keys

-- grammar types
data GrammarSymbol = T Char | NT Char | EPS deriving (Eq, Show)

-- ATN types
type ATN       = [ATNPath]
type ATNPath   = [ATNEdge]
type ATNEdge   = (ATNState, GrammarSymbol, ATNState)
data ATNState  = INIT Char | CHOICE Char Int | MIDDLE Int | FINAL Char
                 deriving (Eq, Ord, Show)
type ATNStack  = [ATNState]
type ATNEnv    = [(GrammarSymbol, ATN)]
type ATNConfig = (ATNState, Int, ATNStack)

-- DFA types
type DFA      = [DFAEdge]
type DFAEdge  = (DFAState, Token, DFAState)
data DFAState = D [ATNConfig] | F Int | Derror deriving (Eq, Show)
type DFAEnv   = [(GrammarSymbol, DFA)]

-- Input sequence type
type InputSeq = [Token]
type Token    = Char

-- Return type of parse function
data AST = Node Char [AST] | Leaf Char deriving (Show)

--------------------------------CONSTANTS---------------------------------------

emptyEnv        = []
emptyStack      = []
emptyDerivation = []

--------------------------------AUXILIARY FUNCTIONS-----------------------------

-- Return the ATN edge with ATN state p on the left
-- Q: Should I handle the "no edge found" case here, or in the caller?
outgoingEdge :: ATNState -> ATNEnv -> Maybe ATNEdge
outgoingEdge p atnEnv = let edges = (concat . concat) (map snd atnEnv)
                        in  find (\(p', t, q) -> p == p') edges

-- Return all ATN edges with ATN state p on the left
outgoingEdges :: ATNState -> ATNEnv -> [ATNEdge]
outgoingEdges p atnEnv = let edges = (concat . concat) (map snd atnEnv)
                         in  filter (\(p', t, q) -> p == p') edges

-- Better way to ensure that the parameter is a D DFA state?
getConflictSetsPerLoc :: DFAState -> [[ATNConfig]]
getConflictSetsPerLoc q =
  case q of
    F _       -> error "final state passed to getConflictSetsPerLoc"
    Derror    -> error "error state passed to getConflictSetsPerLoc"
    D configs -> let sortedConfigs = sortOn (\(p, i, gamma) -> (p, gamma)) configs
                 in  groupBy (\(p, i, gamma) (p', j, gamma') ->
                               p == p' && i /= j && gamma == gamma')
                             sortedConfigs

getProdSetsPerState :: DFAState -> [[ATNConfig]]
getProdSetsPerState q = 
  case q of
    F _       -> error "final state passed to getProdSetsPerState"
    Derror    -> error "error state passed to getProdSetsPerState"
    D configs -> let sortedConfigs = sortOn (\(p, i, gamma) -> (p, gamma)) configs
                 in  groupBy (\(p, _, _) (p', _, _) -> p == p')
                             sortedConfigs

dfaTrans :: DFAState -> Token -> DFA -> Maybe DFAEdge
dfaTrans d t dfa =find (\(d1, label, _) -> d1 == d && label == t) dfa


allEqual :: Eq a => [a] -> Bool
allEqual []       = True
allEqual (x : xs) = all (== x) xs

--------------------------------ALL(*) FUNCTIONS--------------------------------
-- should parse() also return residual input sequence?

parse :: InputSeq -> GrammarSymbol -> ATNEnv -> (Maybe Bool, AST)
parse input startSym atnEnv  =
  let parseLoop input currState stack dfaEnv subtrees astStack =
        case (currState, startSym) of
          (FINAL c, NT c') ->
            if c == c' then
              (Just True, Node c subtrees)
            else
              case (stack, astStack) of
                (q : stack', leftSiblings : astStack') ->
                  parseLoop input q stack' dfaEnv (leftSiblings ++ [Node c subtrees]) astStack'
                _ -> error "this shouldn't happen"
          (_, _) ->
            case (outgoingEdge currState atnEnv) of
              Nothing -> error ("No matching edge found for " ++ (show currState))
              Just (p, t, q) ->
                case (t, input) of
                  (T b, [])     -> error "Input has been exhausted"
                  (T b, c : cs) -> if b == c then
                                     parseLoop cs q stack dfaEnv (subtrees ++ [Leaf b]) astStack
                                   else
                                     (Just False, Leaf 'z')
                  (NT b, _)     -> let stack' = q : stack
                                       i      = adaptivePredict t input stack' dfaEnv
                                   in  parseLoop input (CHOICE b i) stack' dfaEnv [] (subtrees : astStack)
                  (EPS, _)      -> parseLoop input q stack dfaEnv subtrees astStack
                             
      iStart = adaptivePredict startSym input emptyStack emptyEnv
      
  in  case startSym of (NT c) -> parseLoop input (CHOICE c iStart) emptyStack (map (\(sym, _) -> (sym, [])) atnEnv) [] emptyStack
                       _      -> error "Start symbol must be a nonterminal"

  where

    adaptivePredict sym input stack dfaEnv =
      case lookup sym dfaEnv of
        Just dfa -> -1 
        Nothing  -> let d0     = startState sym emptyStack
                        -- Do I actually need to create the final states ahead of time?
                        {-
                        finals = case (lookup sym atnEnv) of
                                   Just ess -> [F i | i <- map fst (zip [0..] ess)]
                                   Nothing  -> trace (error ("aP No ATN for symbol " ++ show sym)) [Derror]
                        -}
                        alt    = sllPredict sym input d0 stack dfaEnv
                    in  alt

    startState sym stack =
      case lookup sym atnEnv of
        Nothing   -> trace (error ("sS No ATN for symbol " ++ show sym)) Derror
        Just atn  ->
          let loopOverAtnPaths atn =
                case atn of
                  []           -> []
                  path : paths ->
                    case path of
                      (INIT _, _, CHOICE ntName i) : edges ->
                        (closure [] (CHOICE ntName i, i, stack)) ++
                        loopOverAtnPaths paths
                      _  ->
                        error "ATN must begin with an edge from an INIT to a CHOICE"
          in  D (loopOverAtnPaths atn)

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
                (_, NT ntName, q) : es' ->
                  closure busy' (INIT ntName, i, q : gamma) ++
                  loopOverEdges es'
                (_, EPS, q) : es'       ->
                  closure busy' (q, i, gamma) ++
                  loopOverEdges es'
                (_, T _, _) : es'       ->
                  loopOverEdges es'
        in  case (p, gamma) of
          (FINAL _, [])         -> [currConfig]
          (FINAL _, q : gamma') -> currConfig : closure busy' (q, i, gamma')
          _                     -> currConfig : loopOverEdges pEdges

    sllPredict sym input d0 stack dfaEnv =
      let predictionLoop d tokens =
            case tokens of
              []     -> llPredict sym input stack -- empty input
              t : ts ->
                let d' = case lookup sym dfaEnv of
                           Nothing  -> error ("No DFA found for nonterminal " ++ show sym ++ show dfaEnv)
                           Just dfa ->
                             case dfaTrans d t dfa of
                               Just (_, _, d2) -> d2
                               Nothing         -> target d t
                in  case d' of
                      Derror            -> error ("No target DFA state found " ++ show d)
                      F i               -> i
                      D atnConfigs      ->
                        let conflictSets   = getConflictSetsPerLoc d'
                            prodSets       = getProdSetsPerState d'
                            stackSensitive =
                              any (\cSet -> length cSet > 1) conflictSets &&
                              not (any (\pSet -> length pSet == 1) prodSets)
                        in  if stackSensitive then
                              llPredict sym input stack
                            else
                              predictionLoop d' ts
      in  predictionLoop d0 input

    llPredict sym input stack =
      let d0 = startState sym stack
          predictionLoop d tokens =
            case tokens of
              []     -> error ("Empty input in llPredict")
              t : ts -> 
                let mv = move d t
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
{-
    target d a =
      let mv = move d a
          d' = D (concat (map (closure []) mv))
      in  case d' of
            D [] -> (Derror, False)
            D atnConfigs ->
              case nub (map (\(_, j, _) -> j) atnConfigs) of
                [i] -> (F i, False)
                _   -> let conflictSets   = getConflictSetsPerLoc d'
                           prodSets       = getProdSetsPerState d'
                           stackSensitive =
                             any (\cSet -> length cSet > 1) conflictSets &&
                             not (any (\pSet -> length pSet == 1) prodSets)
                           ss' = stackSensitive
                       in  (d', ss')
-}

    target d a =
      let mv = move d a
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
                              T a -> if t == a then
                                       (q, i, gamma) : acc
                                     else
                                       acc
                              _   -> acc)
                          []
                          pOutgoingEdges
          in concat (map qsForP atnConfigs)

