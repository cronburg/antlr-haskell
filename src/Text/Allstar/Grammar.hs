module Text.Allstar.Grammar
  ( NonTerminal
  , Terminal
  , ProdElem(..)
  , Symbols
  , Production(..)
  , ProdRHS(..)
  , Predicate
  , Mutator
  , Grammar(..)
  , defaultGrammar
  ) where
import Prelude hiding (pi)
import Data.Set (Set(..), empty)

type NonTerminal = String
type Terminal    = String

-- Grammar Symbols:
data ProdElem    =
    NT NonTerminal
  | T  Terminal

type Symbols     = [ProdElem]

data ProdRHS s =
    Prod     Symbols
  | Sem     (Predicate s) Symbols
  | Action  (Mutator s)

data Production s = Production NonTerminal (ProdRHS s)

-- Predicates and Mutators act over some state
type Predicate s = s -> Bool
type Mutator   s = s -> s

data Grammar s = G
  { ns  :: Set NonTerminal
  , ts  :: Set Terminal
  , ps  :: [Production s]
  , s0  :: NonTerminal
  , pis :: [Predicate s]
  , ms  :: [Mutator   s]
  }

defaultGrammar = G
  { ns  = empty
  , ts  = empty
  , ps  = []
  , s0  = ""
  , pis = []
  , ms  = []
  }

