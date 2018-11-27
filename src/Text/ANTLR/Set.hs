{-# LANGUAGE GADTs, DeriveAnyClass, DeriveGeneric, OverloadedStrings, DeriveLift
    , QuasiQuotes, TemplateHaskell, DeriveDataTypeable #-}
{-|
  Module      : Text.ANTLR.Set
  Description : Entrypoint for swapping out different underlying set representations
  Copyright   : (c) Karl Cronburg, 2018
  License     : BSD3
  Maintainer  : karl@cs.tufts.edu
  Stability   : experimental
  Portability : POSIX

-}
module Text.ANTLR.Set
  ( Set, null, size, member, notMember
  , empty, singleton, insert, delete, union, unions
  , difference, intersection, filter, map, foldr, foldl', fold
  , toList, fromList, (\\), findMin, maybeMin
  , Hashable(..), Generic(..)
  ) where
import Text.ANTLR.Pretty

import GHC.Generics (Generic, Rep)
import Data.Hashable (Hashable(..))
import Language.Haskell.TH.Syntax (Lift(..))

import qualified Data.Functor         as F
import qualified Control.Applicative  as A
import qualified Data.Foldable        as Foldable

import Data.Map ( Map(..) )
import qualified Data.Map as M

import qualified Data.HashSet as S
import Data.HashSet as S
  ( HashSet(..), member, toList, union
  , null, empty, map, size, singleton, insert
  , delete, unions, difference, intersection, foldl'
  , fromList
  )

import Prelude hiding (null, filter, map, foldr, foldl)

-- | Use a hash-based set (hashable keys) for our internal set representation
--   during parsing.
type Set = S.HashSet

-- | Is @e@ not a member of the set @s@.
notMember e s = not $ member e s

-- | Set fold
fold = S.foldr

-- | Set fold
foldr = S.foldr

-- | Find the minimum value of an orderable set.
findMin :: (Ord a, Hashable a) => Set a -> a
findMin = minimum . toList

--maybeMin :: (Ord a, Hashable a) => Set a -> Maybe a
-- | Get minimum of a set without erroring out on empty set.
maybeMin as
  | S.size as == 0  = Nothing
  | otherwise       = Just $ findMin as

infixl 9 \\

-- | Set difference
(\\) :: (Hashable a, Eq a) => Set a -> Set a -> Set a
m1 \\ m2 = difference m1 m2

instance (Hashable a, Eq a, Lift a) => Lift (S.HashSet a) where
  lift set = [| fromList $(lift $ toList set) |]

instance (Hashable k, Hashable v) => Hashable (Map k v) where
  hashWithSalt salt mp = salt `hashWithSalt` M.toList mp

instance (Prettify a, Hashable a, Eq a) => Prettify (S.HashSet a) where
  prettify s = do
    pStr "Set: "; incrIndent 5
    pListLines $ toList s
    incrIndent (-5)
    pLine ""

--filter :: (Hashable a, Eq a) => (a -> Bool) -> Set a -> Set a
-- | Set filter
filter f s = S.filter f s

--instance (Hashable a, Eq a) => Hashable (S.HashSet a) where
--  hashWithSalt salt set = salt `hashWithSalt` S.toList (run set)


{-

--import Data.Set.Monad (Set(..), member, toList, union, notMember)
--import qualified Data.Set.Monad as Set

import Prelude hiding (null, filter, map, foldr, foldl)
import qualified Data.List            as L
--import qualified Data.Set             as S
import qualified Data.Functor         as F
import qualified Control.Applicative  as A
import qualified Data.Foldable        as Foldable

import Data.Monoid
import Data.Foldable (Foldable)
import Control.Arrow
import Control.Monad
import Control.DeepSeq

import Data.Hashable (Hashable(..))
import GHC.Generics (Generic, Rep)
import Control.DeepSeq (NFData(..))
import Language.Haskell.TH.Syntax (Lift(..))
import Data.Data (Data(..))

import Data.Map ( Map(..) )
import qualified Data.Map as M

import Text.ANTLR.Pretty

instance (Hashable k, Hashable v) => Hashable (Map k v) where
  hashWithSalt salt mp = salt `hashWithSalt` M.toList mp

instance (Hashable a, Eq a) => Hashable (Set a) where
  hashWithSalt salt set = salt `hashWithSalt` S.toList (run set)

instance (Hashable a, Ord a) => Ord (Set a) where
  s1 <= s2 = S.toList (run s1) <= S.toList (run s2)

data Set a where
  Prim   :: (Hashable a, Eq a) => S.HashSet a -> Set a
  Return :: a -> Set a
  Bind   :: Set a -> (a -> Set b) -> Set b
  Zero   :: Set a
  Plus   :: Set a -> Set a -> Set a

instance (Data a) => Data (Set a)

instance (Hashable a, Eq a, Lift a) => Lift (Set a) where
  lift set = [| fromList $(lift $ toList set) |]

run :: (Hashable a, Eq a) => Set a -> S.HashSet a
run (Prim s)                        = s
run (Return a)                      = S.singleton a
run (Zero)                          = S.empty
run (Plus ma mb)                    = run ma `S.union` run mb
run (Bind (Prim s) f)               = S.foldl' S.union S.empty (S.map (run . f) s)
run (Bind (Return a) f)             = run (f a)
run (Bind Zero _)                   = S.empty
run (Bind (Plus (Prim s) ma) f)     = run (Bind (Prim (s `S.union` run ma)) f)
run (Bind (Plus ma (Prim s)) f)     = run (Bind (Prim (run ma `S.union` s)) f)
run (Bind (Plus (Return a) ma) f)   = run (Plus (f a) (Bind ma f))
run (Bind (Plus ma (Return a)) f)   = run (Plus (Bind ma f) (f a))
run (Bind (Plus Zero ma) f)         = run (Bind ma f)
run (Bind (Plus ma Zero) f)         = run (Bind ma f)
run (Bind (Plus (Plus ma mb) mc) f) = run (Bind (Plus ma (Plus mb mc)) f)
run (Bind (Plus ma mb) f)           = run (Plus (Bind ma f) (Bind mb f))
run (Bind (Bind ma f) g)            = run (Bind ma (\a -> Bind (f a) g))

instance F.Functor Set where
  fmap = liftM

instance A.Applicative Set where
  pure  = return
  (<*>) = ap

instance A.Alternative Set where
  empty = Zero
  (<|>) = Plus

instance Monad Set where
  return = Return
  (>>=)  = Bind

instance MonadPlus Set where
  mzero = Zero
  mplus = Plus

instance (Hashable a, Eq a) => Monoid (Set a) where
  mempty  = empty
  mappend = union
  mconcat = unions

instance Foldable Set where
    foldr f def m = 
        case m of
            Prim s -> S.foldr f def s
            Return a -> f a def
            Zero -> def
            Plus ma mb -> Foldable.foldr f (Foldable.foldr f def ma) mb
            Bind s g -> Foldable.foldr f' def s
                where f' x b = Foldable.foldr f b (g x)

instance (Hashable a, Eq a) => Eq (Set a) where
  s1 == s2 = run s1 == run s2

--instance (Hashable a, Eq a, Ord a) => Ord (Set a) where
--  compare s1 s2 = compare (run s1) (run s2)

instance (Show a, Hashable a, Eq a) => Show (Set a) where
  show = show . run

instance (Prettify a, Hashable a, Eq a) => Prettify (Set a) where
  prettify s = do
    pStr "Set: "; incrIndent 5
    pListLines $ toList s
    incrIndent (-5)
    pLine ""

instance (Read a, Hashable a, Eq a) => Read (Set a) where
  readsPrec i s = L.map (first Prim) (readsPrec i s)

instance (NFData a, Hashable a, Eq a) => NFData (Set a) where
  rnf = rnf . run

infixl 9 \\

(\\) :: (Hashable a, Eq a) => Set a -> Set a -> Set a
m1 \\ m2 = difference m1 m2

null :: (Hashable a, Eq a) => Set a -> Bool
null = S.null . run

size :: (Hashable a, Eq a) => Set a -> Int
size = S.size . run

member :: (Hashable a, Eq a) => a -> Set a -> Bool
member a s = S.member a (run s)

notMember :: (Hashable a, Eq a) => a -> Set a -> Bool
notMember a t = not (member a t)

empty :: (Hashable a, Eq a) => Set a
empty = Prim S.empty

singleton :: (Hashable a, Eq a) => a -> Set a
singleton a = Prim (S.singleton a)

insert :: (Hashable a, Eq a) => a -> Set a -> Set a
insert a s = Prim (S.insert a (run s))

delete :: (Hashable a, Eq a) => a -> Set a -> Set a
delete a s = Prim (S.delete a (run s))

union :: (Hashable a, Eq a) => Set a -> Set a -> Set a
union s1 s2 = Prim (run s1 `S.union` run s2)

unions :: (Hashable a, Eq a) => [Set a] -> Set a
unions ss = Prim (S.unions (L.map run ss))

difference :: (Hashable a, Eq a) => Set a -> Set a -> Set a
difference s1 s2 = Prim (S.difference (run s1) (run s2))

intersection :: (Hashable a, Eq a) => Set a -> Set a -> Set a
intersection s1 s2 = Prim (S.intersection (run s1) (run s2))

filter :: (Hashable a, Eq a) => (a -> Bool) -> Set a -> Set a
filter f s = Prim (S.filter f (run s))

map :: (Hashable a, Eq a, Hashable b, Eq b) => (a -> b) -> Set a -> Set b
map f s = Prim (S.map f (run s))

foldr :: (Hashable a, Eq a) => (a -> b -> b) -> b -> Set a -> b
foldr f z s = S.foldr f z (run s)

fold :: (Hashable a, Eq a) => (a -> b -> b) -> b -> Set a -> b
fold f z s = S.foldr f z (run s)

foldl' :: (Hashable a, Eq a) => (b -> a -> b) -> b -> Set a -> b
foldl' f z s = S.foldl' f z (run s)

toList :: (Hashable a, Eq a) => Set a -> [a]
toList = S.toList . run

fromList :: (Hashable a, Eq a) => [a] -> Set a
fromList as = Prim (S.fromList as)

findMin :: (Ord a, Hashable a) => Set a -> a
findMin = minimum . toList

maybeMin :: (Ord a, Hashable a) => Set a -> Maybe a
maybeMin as
  | size as == 0 = Nothing
  | otherwise    = Just $ findMin as

-}

