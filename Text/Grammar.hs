module Text.Grammar where

-- !!TARA!! Need to define full token class
type Token       = String
type NonTerminal = String
type Terminal    = Token
data ProdElement = Nonterminal | Terminal
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

