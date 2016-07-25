module Text.AllstarTest where

--Need to define full token class
type Token       = String
type NonTerminal = String
type Terminal    = Token
data ProdElement = Nonterminal | Terminal
-- ContextFree: A -> alpha_i is the i-th context-free production of A
--              alpha is a a sequence of production elements.
data Production  = ContextFree NonTerminal [[ProdElement]]
                 | Predicated  NonTerminal [(Maybe Predicate)] [[ProdElement]]
                 | Mutated     NonTerminal [Mutator]
                 | Epsilon     NonTerminal
type Predicate   = (State -> Bool)
type Mutator     = (State -> State)
type State       = undefined

data Grammar = Grammar { gN  :: [NonTerminal]
                       , gT  :: [Terminal]
                       , gP  :: [Production]
                       , gS  :: NonTerminal
                       , gPi :: [Predicate]
                       , gM  :: [Mutator]
                       }
  deriving (Show, Eq)
type StateLabel = String
data State = EntryState NodeLabel [Edge]
           | MidState NodeLabel [Edge]
           | FinalState NodeLabel
data EdgeLabel = NTLabel NonTerminal
           | TLabel  Terminal
           | PLabel  Predicate
           | MLabel  Mutator
           | Epsilon
type Transition = (State, EdgeLabel, State)

-- ATN M_G = (Q, Sigma, Delta, E, F)
-- Q is the set of states (atnQ). ATN states are represented as nodes.
-- Sigma is the edge alphabet N U T U Pi U M (atnSigma)
-- Delta is the transition relation mapping Q x (Sigma U e) -> Q (atnDelta)
--       Transitions are represented as edges.
-- E is the set of submachine entry states (stored in NodeType as Entry)
-- F is the set of submachine final states (stored in NodeType as Final)
data ATN    = ATN { atnQ     :: [State]
                   , atnSigma :: [EdgeLabel]
                   , atnDelta :: [Transition]
                   , atnE     :: [Node]
                   , atnF     :: [Node]
                   }

-- q notes
-- for each nonterminal we have 1 start node and 1 end node
-- for each production we have 1 dummy start node
-- for each context free production we have 1 node for each grammar element in the grammar string
-- for each predicated production we have 1 node for the predicate and 1 node for each grammar element
-- for each mutated production we have 0 extra nodes
-- for each episilon production we have - extra nodes
-- sigma notes
-- iterate over N T Pi and M to grab all the labels
-- delta
-- for each production make edges between the nodes named in q
  -- optimization could do this when mapping over the productions

-- INPUT: Grammar g
-- OUTPUT: ATN corresponding to g
toAtn :: Grammar -> ATN
toAtn g =
  let parseP []        (q, d, e, f) = (q, d, e, f)
           | (prod:_)  (q, d, e, f) = 
                case prod of 
                    ContextFree nt (prodElem:_)          ->
                    Predicated  nt (pred:_) (prodElem:_) ->
                    Mutated     nt (mut:_)               ->
                    Epsilon     nt                       ->
                           
      (q, d, e, f) = parseP (gP g) ([], [], [], [])
  in  ATN q sigma d e f
