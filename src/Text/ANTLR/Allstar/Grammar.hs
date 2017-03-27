{-# LANGUAGE ScopedTypeVariables #-}
module Text.ANTLR.Allstar.Grammar
  ( NonTerminal
  , Terminal
  , sameNTs, sameTokens
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
  , validGrammar, hasAllNonTerms, hasAllTerms, startIsNonTerm
  , symbols
  ) where
import Prelude hiding (pi)
import Data.Set (Set(..), empty, fromList, member, (\\), intersection
  , union
  )
import qualified Data.Set as S

----------------------------------------------------------------
-- When we *Show* production elements, they should contain source location
-- information, but when we *compare* them, we should ignore the source info.

class NonTerminal nt where
  sameNTs :: nt -> nt -> Bool

class Terminal t where
  sameTokens :: t -> t -> Bool

-- Grammar Symbols:
data ProdElem nt t =
    NT nt
  | T  t
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

type Symbols nt t = [ProdElem nt t]

data ProdRHS s nt t =
    Prod    (Symbols nt t)
  | Sem     (Predicate s) (Symbols nt t)
  | Action  (Mutator s)
  deriving (Eq, Ord, Show)

isProd (Prod _) = True
isProd _ = False

isSem (Sem _ _) = True
isSem _ = False

isAction (Action _) = True
isAction _ = False

getProds = map (\(Prod ss) -> ss) . filter isProd

type Production s nt t = (nt, ProdRHS s nt t)

-- Get only the productions for the given nonterminal nt:
prodsFor :: forall s. forall nt. forall t. (NonTerminal nt, Terminal t) => Grammar s nt t -> nt -> [Production s nt t]
prodsFor g nt = let
    matchesNT :: Production s nt t -> Bool
    matchesNT (nt', _) = sameNTs nt' nt
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

data Grammar s nt t = G
  { ns  :: Set nt
  , ts  :: Set t
  , ps  :: [Production s nt t]
  , s0  :: nt
  , _πs :: Set (Predicate s)
  , _μs :: Set (Mutator   s)
  } deriving (Eq, Ord, Show)

symbols :: (NonTerminal nt, Terminal t, Ord nt, Ord t) => Grammar s nt t -> Set (ProdElem nt t)
symbols g = S.insert Eps $ S.map NT (ns g) `union` S.map T (ts g)

defaultGrammar = G
  { ns  = empty
  , ts  = empty
  , ps  = []
  , s0  = ""
  , _πs = empty
  , _μs = empty
  }

validGrammar :: (NonTerminal nt, Terminal t, Eq nt, Ord nt, Eq t, Ord t) => Grammar s nt t -> Bool
validGrammar g =
     hasAllNonTerms g
  && hasAllTerms g
  && startIsNonTerm g
--  && distinctTermsNonTerms g

hasAllNonTerms :: (NonTerminal nt, Terminal t, Eq nt, Ord nt) => Grammar s nt t -> Bool
hasAllNonTerms g =
  ns g == (fromList . getNTs . concat . getProds . map snd $ ps g)

hasAllTerms :: (NonTerminal nt, Terminal t, Eq t, Ord t) => Grammar s nt t -> Bool
hasAllTerms g =
  ts g == (fromList . getTs . concat . getProds . map snd $ ps g)

startIsNonTerm :: (NonTerminal nt, Terminal t, Ord nt) => Grammar s nt t -> Bool
startIsNonTerm g = s0 g `member` ns g

--distinctTermsNonTerms g =
--  (ns g `intersection` ts g) == empty

