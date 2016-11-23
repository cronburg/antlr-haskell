module Text.Allstar.Grammar
  ( NonTerminal
  , Terminal
  , Production(..)
  , Predicate
  , Mutator
  , Grammar(..)
  ) where
import Prelude hiding (pi)
import Data.Set (Set(..), empty)

type NonTerminal = String
type Terminal    = String

data Production  = Prod
  NonTerminal
  [Either NonTerminal Terminal]

type Predicate   = Parser -> Bool
type Mutator     = Parser -> Parser

data Grammar = G 
  { n  :: Set NonTerminal
  , t  :: Set Terminal
  , p  :: [Production]
  , s  :: NonTerminal
  , pi :: [Predicate]
  , m  :: [Mutator]
  }

defaultGrammar = G
  { n  = empty
  , t  = empty
  , p  = []
  , s  = ""
  , pi = []
  , m  = []
  }

