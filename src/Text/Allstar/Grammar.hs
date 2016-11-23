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

-- Predicates and Mutators act over some state
type Predicate s = s -> Bool
type Mutator   s = s -> s

data Grammar s = G
  { n  :: Set NonTerminal
  , t  :: Set Terminal
  , p  :: [Production]
  , s  :: NonTerminal
  , pi :: [Predicate s]
  , m  :: [Mutator   s]
  }

defaultGrammar = G
  { n  = empty
  , t  = empty
  , p  = []
  , s  = ""
  , pi = []
  , m  = []
  }

