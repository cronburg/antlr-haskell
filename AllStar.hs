module AllStar where

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
-- to do: add transition function field to DFA when it's needed
--        consider defining DFA solely in terms of transition function
data DFA      = DFA { initialState :: DFAState
                    , finalStates  :: [DFAState]
                    , sigma        :: [Char]
                    }
data DFAState = D [ATNConfig] | F Int | Derror deriving (Eq, Show)
type DFAEnv   = [(GrammarSymbol, DFA)]

-- Input sequence type (this will probably change to [Token])
type InputSeq = [Token]
type Token    = Char

--------------------------------CONSTANTS---------------------------------------

emptyEnv    = []
emptyStack  = []
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

allEqual :: Eq a => [a] -> Bool
allEqual []       = True
allEqual (x : xs) = all (== x) xs

--------------------------------ALL(*) FUNCTIONS--------------------------------
-- should parse() also return residual input sequence?

parse :: InputSeq -> GrammarSymbol -> ATNEnv -> (Maybe Bool, [[GrammarSymbol]])
parse input startSym atnEnv  =
  let parseLoop input currState stack dfaEnv derivation =
        case (currState, startSym) of
          (FINAL c, NT c') -> if c == c' then
                                (Just True, derivation)
                              else
                                case stack of
                                  []         -> error "Can't pop from an empty stack"
                                  q : stack' -> parseLoop input q stack' dfaEnv derivation
          (_, _) -> case (outgoingEdge currState atnEnv) of
                      Nothing -> error ("No matching edge found for " ++ (show currState))
                      Just (p, t, q) ->
                        case (t, input) of
                          (T b, [])     -> error "Input has been exhausted"
                          (T b, c : cs) -> if b == c then
                                             let newDerivationStep = case last (last derivation) of
                                                                     NT _ -> init (last derivation) ++ [t]
                                                                     T _  -> last derivation ++ [t]
                                             in  parseLoop cs q stack dfaEnv (derivation ++ [newDerivationStep])
                                           else
                                             (Just False, derivation)
                          (NT b, _)     -> let stack' = q : stack
                                               i      = adaptivePredict t input stack' dfaEnv
                                               newDerivationStep = case last (last derivation) of
                                                                     NT _ -> init (last derivation) ++ [t]
                                                                     T _  -> last derivation ++ [t]
                                           in  parseLoop input (CHOICE b i) stack' dfaEnv (derivation ++ [newDerivationStep])
                          (EPS, _)      -> parseLoop input q stack dfaEnv derivation
                             
      iStart = adaptivePredict startSym input emptyStack emptyEnv
      
  in  case startSym of (NT c) -> parseLoop input (CHOICE c iStart) emptyStack emptyEnv [[startSym]]
                       _      -> error "Start symbol must be a nonterminal"

  where

    adaptivePredict sym input stack dfaEnv =
      case lookup sym dfaEnv of
        Just dfa -> -1 
        Nothing  -> let d0     = startState sym emptyStack
                        finals = case (lookup sym atnEnv) of
                                   Just ess -> [F i | i <- map fst (zip [0..] ess)]
                                   Nothing  -> trace (error ("aP No ATN for symbol " ++ show sym)) [Derror]
                        dfa    = DFA { initialState = d0
                                     , finalStates = finals
                                     , sigma = ['a', 'b', 'c']
                                     }  --derive alphabet from the ATNs?
                        alt    = sllPredict sym input d0 stack
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

    sllPredict sym input d0 stack =
      let predictionLoop d tokens =
            case (d, tokens) of
              (Derror, _)            -> error ("No target DFA state found " ++
                                               show d)
              (F i, _)               -> i
              (D atnConfigs, [])     -> llPredict sym input stack --error ("Empty input in sllPredict, " ++ "DFA cursor is " ++ show d)
              (D atnConfigs, t : ts) ->
                let (d', stackSensitive) = target d t
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

