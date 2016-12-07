module Text.ANTLR.Allstar.Grammar
  ( NonTerminal
  , Terminal
  , ProdElem(..)
  , Symbols
  , Production(..)
  , ProdRHS(..)
  , Predicate(..)
  , Mutator(..)
  , Grammar(..)
  , defaultGrammar
  , isProd, isSem, isAction
  , isNT, isT, isEps, getNTs, getTs, getEps
  , prodsFor, getProds
) where
import Prelude hiding (pi)
import Data.Set (Set(..), empty)

----------------------------------------------------------------
-- When we *Show* production elements, they should contain source location
-- information, but when we *compare* them, we should ignore the source info.

type NonTerminal = String
type Terminal    = String

-- Grammar Symbols:
data ProdElem    =
    NT NonTerminal
  | T  Terminal
  | Eps
  deriving (Eq, Ord, Show)

isNT (NT _) = True
isNT _      = False

isT (T _) = True
isT _     = False

isEps Eps = True
isEps _   = False

getNTs = map (\(NT nt) -> nt) . filter isNT
getTs  = map (\(T t) -> t) . filter isT
getEps = map (\Eps -> Eps) . filter isEps -- no

type Symbols     = [ProdElem]

data ProdRHS s =
    Prod     Symbols
  | Sem     (Predicate s) Symbols
  | Action  (Mutator s)
  deriving (Eq, Ord, Show)

isProd (Prod _) = True
isProd _ = False

isSem (Sem _ _) = True
isSem _ = False

isAction (Action _) = True
isAction _ = False

getProds = map (\(Prod ss) -> ss) . filter isProd

type Production s = (NonTerminal, ProdRHS s)

-- Get only the productions for the given nonterminal nt:
prodsFor :: Grammar a -> NonTerminal -> [Production a]
prodsFor g nt = let
    matchesNT :: Production a -> Bool
    matchesNT (nt', _) = nt' == nt
  in filter matchesNT (ps g)

-- Predicates and Mutators act over some state. The String
-- identifiers should eventually correspond to source-level
-- e.g. location / allocation site information, i.e. two
-- predicates or mutators are equivalent iff they were
-- constructed from the same production rule.
data Predicate s = Predicate String (s -> Bool)

instance Eq (Predicate s) where
  Predicate p1 _ == Predicate p2 _ = p1 == p2
instance Ord (Predicate s) where
  Predicate p1 _ `compare` Predicate p2 _ = p1 `compare` p2
instance Show (Predicate s) where
  show (Predicate p1 _) = show p1

data Mutator   s = Mutator String (s -> s)

instance Eq (Mutator s) where
  Mutator m1 _ == Mutator m2 _ = m1 == m2
instance Ord (Mutator s) where
  Mutator m1 _ `compare` Mutator m2 _ = m1 `compare` m2
instance Show (Mutator s) where
  show (Mutator m1 _) = show m1

data Grammar s = G
  { ns  :: Set NonTerminal
  , ts  :: Set Terminal
  , ps  :: [Production s]
  , s0  :: NonTerminal
  , _πs :: Set (Predicate s)
  , _μs :: Set (Mutator   s)
  } deriving (Eq, Ord, Show)

defaultGrammar = G
  { ns  = empty
  , ts  = empty
  , ps  = []
  , s0  = ""
  , _πs = empty
  , _μs = empty
  }

