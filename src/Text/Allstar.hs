module Allstar where
-- Set
import Data.Set (Set)
import qualified Data.Set as Set

-- data types
-- State API
data State = State Transition | SuperState Transition [State]
getConfs :: State -> [Conf]
getConfs = undefined
transitionOf = undefined
terminalAt i = undefined
isStopState = undefined
ruleOf = undefined
startRuleOf = undefined

data Transition = Terminal Terminal State
                | NonTerminal NonTerminal State
                | Action Action State
                | Predicate Predicate State
                | Epsilon State

type Action = Parser -> Parser
type Predicate = Parser -> Bool
data Terminal
data NonTerminal

data ParserMember = State State | WildCard
type ParserStack = [ParserMember]
data Parser = Parser Grammar ATN
data Grammar
data ATN
hasPredicatedProductionFor :: Grammar -> NonTerminal -> Bool
hasPredicatedProductionFor = undefined

epsilonTransitionOf = undefined
atnOf :: Parser -> ATN
atnOf = undefined

type Conf = Conf { cTerm :: Terminal
                 , cIndex :: Index
                 , cStack :: ParserStack }
--depends on: adaptivePredict
parse :: Parser -> NonTerminal -> Parser
parse parser start =
  let gamma = emptyStack
      i = adaptivePredict s gamma
      p = s `terminalAt` i
      parse' :: Parser -> NonTerminal -> ParserStack -> State -> Parser
      parse' parser' g' p' i' =
        if isStopState p'
        then if (ruleOf p) == start
             then parser
             else let (q,g'') = pop g'
                  in  parse' parser' g'' q i'
        else
          let (parser'' ,g'',p'',i'') =
                case transitionOf p' of
                  Terminal b q   ->
                    if b == currentInput s
                    then (advanceInput parser', g', q, i')
                    else error "terminal transition parse error"
                  NonTerminal b q ->
                    let i'' = adaptivePredict b g'
                    in  (parser',push g' q ,b `stateAt` i'', i'' )
                  Action     mu q ->
                    (mu parser', g', q, i')
                  Predicate  pi q ->
                    if pi parser'
                    then (parser', g', q, i')
                    else error "Parser failed predicate"
                  Epsilon    q    -> (parser', g', q, i')
          in  parse' s'' g'' p'' i''
  in parse' s gamma p i

--depends on: llPredict
--            sllPredict
--            startState
adaptivePredict :: Parser -> NonTerminal -> ParserStack -> Index
adaptivePredict parser a gamma0 =
  if   (grammarOf parser) `hasPredicatedProductionFor` a
  then llPredict a (indexOf parser)  gamma0
  else let
           d0 = startState parser a [WildCard]
           fDFA = undefined
           qDFA = undefined
           aDFA = undefined
       in  sllPredict a d0 (indexOf parser) gamma0




--depends on: closure
startState :: Parser -> NonTerminal -> ParserStack -> State
startState parser a gamma =
  let d0 = mkSuperState
      accState transition state =
        --if pA -e> pA,i -pi_i> p then pi := pi_i else pi := epsilon
        --if pi=epsilon or eval(pi_i) then D0+=closure({},D0,(pA,i, i, gamma))
  in  foldr accState d0 $ (atnOf parser) `epsilonTransitionOf` a -- return D0


--depends on: target
--            llPredict
sllPredict :: -> NonTerminal -> State -> Index -> ParserStack -> Index
sllPredict = undefined

--depends on: move
--            closure
--            getConflictSetsPerLoc
--            getProdSetsPerState
target :: State -> Terminal -> State
target d a = undefined
-- no dependencies
-- set of all (q,i,Gamma) s.t. p -a> q and (p,i,Gamma) in State d
move :: State -> Terminal -> [Conf]
move d a =
  let
      dConfs :: [Conf]
      dConfs = getConfs d -- all (p,i,Gamma) in d
      reachableByA :: Conf -> Bool
      reachableByA c = undefined
  in filter reachableByA dConfs


--depends on: move
--            closure
--            getConflictSetsPerLoc
llPredict = undefined
--no fn dependencies
closure :: [Conf] -> Conf -> [Conf]
closure busy c =

-- no dependencies
-- for each p,Gamma: get set of alts {i} from (p,-,Gamma) in D Confs
getConflictSetsPerLoc :: State -> [[Index]]
getConflictSetsPerLoc d = undefined

-- no dependencies
-- for each p return set of alts i from (p,-,-) in D Confs
getProdSetsPerState :: State -> [Index]
getProdSetsPerState d s= undefined
