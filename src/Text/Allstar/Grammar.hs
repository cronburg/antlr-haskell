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
  deriving (Eq, Show)

-- Predicates and Mutators act over some state
type Predicate s = s -> Bool
type Mutator   s = s -> s

data Grammar s = G
  { ns  :: Set NonTerminal
  , ts  :: Set Terminal
  , ps  :: Set Production
  , s0  :: NonTerminal
  , pis :: [Predicate s]
  , ms  :: [Mutator   s]
  }

defaultGrammar = G
  { ns  = empty
  , ts  = empty
  , ps  = empty
  , s0  = ""
  , pis = []
  , ms  = []
  }

