module Text.AllstarTest where
type Token       = String
type NonTerminal = String
type Terminal    = Token
data Production  = ContextFree NonTerminal [Either Nonterminal Terminal]
                 | Predicated  NonTerminal (Maybe Predicate) [Either Nonterminal Terminal]
                 | Mutated     NonTerminal Mutator
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
data NodeType = Entry | Final | Vanilla
data Node  = Node NodeType String Bool
data Label = NTLabel NonTerminal
           | TLabel  Terminal
           | PLabel Predicate
           | MLabel Mutator
type Edge = (String, Label, String)

data ATN     = ATN { atnQ     :: [Node]
                   , atnSigma :: [Label]
                   , atnDelta :: [Edge]
                   }
-- getNodeByName :: ATN -> String -> Node
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
toAtn :: Grammar -> ATN
toAtn g =
  let parseQ :: [Node]
      parseQ =
      parseSigma :: [Label]
      parseSigma =
      parseDelta :: [Edge]
      parseDelta =
  in ATN parseQ parseSigma parseDelta


--example
dumbGrammar :: Grammar
dumbGrammar = Grammar { gN = ["S", "A", "B", "I", "D"]
                      , gT = ["1","2","3","+","-","*"]
                      , gS = "S"
                      , gP = [ ContextFree "S" [Left "A"]
                             , ContextFree "S" [Left "B"]
                             , ContextFree "S" [Left "D"]
                             , ContextFree "A" [Left "I", Right "+", Left "I"]
                             , ContextFree "B" [Left "I", Right "-", Left "I"]
                             , ContextFree "I" [Right "1"]
                             , ContextFree "I" [Right "2"]
                             , ContextFree "I" [Right "3"]
                             , Predicated "D" (Just $ \_ -> True) [Left "I", Right "*", Left "I"]
                             ]
                      , gPi = [(\_ -> True)]
                      , gM  = []}
