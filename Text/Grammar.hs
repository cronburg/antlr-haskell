module Text.Grammar where

-- !!TARA!! Need to define full token class
type Token       = String
type NonTerminal = String
type Terminal    = Token
data GrammarSymbol = NT NonTerminal | T Terminal
data ProdElem    = ContextFree [GrammarSymbol]
                 | Predicated (Maybe Predicate) [GrammarSymbol]
                 | Mutated    Mutator
                 | Epsilon
type Production  = (NonTerminal, [ProdElem])
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

