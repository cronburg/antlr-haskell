{-# LANGUAGE Safe  #-}
{-# LANGUAGE GADTs #-}

{-|

The @set-monad@ library exports the @Set@ abstract data type and
set-manipulating functions. These functions behave exactly as their namesakes
from the @Data.Set@ module of the @containers@ library. In addition, the
@set-monad@ library extends @Data.Set@ by providing @Functor@, @Applicative@,
@Alternative@, @Monad@, and @MonadPlus@ instances for sets.

In other words, you can use the @set-monad@ library as a drop-in replacement
for the @Data.Set@ module of the @containers@ library and, in addition, you
will also get the aforementioned instances which are not available in the
@containers@ package.

It is not possible to directly implement instances for the aforementioned
standard Haskell type classes for the @Set@ data type from the @containers@
library. This is because the key operations @map@ and @union@, are constrained
with @Ord@ as follows.

> map :: (Ord a, Ord b) => (a -> b) -> Set a -> Set b
> union :: (Ord a) => Set a -> Set a -> Set a

The @set-monad@ library provides the type class instances by wrapping the
constrained @Set@ type into a data type that has unconstrained constructors
corresponding to monadic combinators. The data type constructors that
represent monadic combinators are evaluated with a constrained run function.
This elevates the need to use the constraints in the instance definitions
(this is what prevents a direct definition). The wrapping and unwrapping
happens internally in the library and does not affect its interface.

For details, see the rather compact definitions of the @run@ function and
type class instances. The left identity and associativity monad laws play a
crucial role in the definition of the @run@ function. The rest of the code
should be self explanatory.

The technique is not new. This library was inspired by [1]. To my knowledge,
the original, systematic presentation of the idea to represent monadic
combinators as data is given in [2]. There is also a Haskell library that
provides a generic infrastructure for the aforementioned wrapping and
unwrapping [3].

The @set-monad@ library is particularly useful for writing set-oriented code
using the do and/or monad comprehension notations. For example, the following
definitions now type check.

> s1 :: Set (Int,Int)
> s1 = do a <- fromList [1 .. 4]
>         b <- fromList [1 .. 4]
>         return (a,b)

> -- with -XMonadComprehensions
> s2 :: Set (Int,Int)
> s2 = [ (a,b) | (a,b) <- s1, even a, even b ]

> s3 :: Set Int
> s3 = fmap (+1) (fromList [1 .. 4])

As noted in [1], the implementation technique can be used for monadic
libraries and EDSLs with restricted types (compiled EDSLs often restrict the
types that they can handle). Haskell's standard monad type class can be used
for restricted monad instances. There is no need to resort to GHC extensions
that rebind the standard monadic combinators with the library or EDSL specific
ones.

@[@1@]@ CSDL Blog: The home of applied functional programming at KU. Monad
Reification in Haskell and the Sunroof Javascript compiler.
<http://www.ittc.ku.edu/csdlblog/?p=88>

@[@2@]@ Chuan-kai Lin. 2006. Programming monads operationally with Unimo. In
Proceedings of the eleventh ACM SIGPLAN International Conference on Functional
Programming (ICFP '06). ACM.

@[@3@]@ Heinrich Apfelmus. The operational package.
<http://hackage.haskell.org/package/operational>

-}


module Data.Set.Monad (
  -- * Set type
  Set
  -- * Operators
  , (\\)

  -- * Query
  , null
  , size
  , member
  , notMember
  , isSubsetOf
  , isProperSubsetOf

  -- * Construction
  , empty
  , singleton
  , insert
  , delete

  -- * Combine
  , union
  , unions
  , difference
  , intersection

  -- * Filter
  , filter
  , partition
  , split
  , splitMember

  -- * Map
  , map
  , mapMonotonic

  -- * Folds
  , foldr
  , foldl
  -- ** Strict folds
  , foldr'
  , foldl'
  -- ** Legacy folds
  , fold

  -- * Min\/Max
  , findMin
  , findMax
  , deleteMin
  , deleteMax
  , deleteFindMin
  , deleteFindMax
  , maxView
  , minView

  -- * Conversion

  -- ** List
  , elems
  , toList
  , fromList

  -- ** Ordered list
  , toAscList
  , fromAscList
  , fromDistinctAscList

  -- * Debugging
  , showTree
  , showTreeWith
  , valid
  ) where

import Prelude hiding (null, filter, map, foldr, foldl)
import qualified Data.List            as L
import qualified Data.Set             as S
import qualified Data.Functor         as F
import qualified Control.Applicative  as A
import qualified Data.Foldable        as Foldable

import Data.Foldable (Foldable)
import Control.Arrow
import Control.Monad
import Control.DeepSeq

data Set a where
  Prim   :: (Ord a) => S.Set a -> Set a
  Return :: a -> Set a
  Bind   :: Set a -> (a -> Set b) -> Set b
  Zero   :: Set a
  Plus   :: Set a -> Set a -> Set a

run :: (Ord a) => Set a -> S.Set a
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

instance Semigroup (Set a) where
  (<>) = Plus

instance (Ord a) => Monoid (Set a) where
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

instance (Ord a) => Eq (Set a) where
  s1 == s2 = run s1 == run s2

instance (Ord a) => Ord (Set a) where
  compare s1 s2 = compare (run s1) (run s2)

instance (Show a, Ord a) => Show (Set a) where
  show = show . run

instance (Read a, Ord a) => Read (Set a) where
  readsPrec i s = L.map (first Prim) (readsPrec i s)

instance (NFData a, Ord a) => NFData (Set a) where
  rnf = rnf . run

infixl 9 \\

(\\) :: (Ord a) => Set a -> Set a -> Set a
m1 \\ m2 = difference m1 m2

null :: (Ord a) => Set a -> Bool
null = S.null . run

size :: (Ord a) => Set a -> Int
size = S.size . run

member :: (Ord a) => a -> Set a -> Bool
member a s = S.member a (run s)

notMember :: (Ord a) => a -> Set a -> Bool
notMember a t = not (member a t)

isSubsetOf :: Ord a => Set a -> Set a -> Bool
isSubsetOf s1 s2 = S.isSubsetOf (run s1) (run s2)

isProperSubsetOf :: Ord a => Set a -> Set a -> Bool
isProperSubsetOf s1 s2 = S.isProperSubsetOf (run s1) (run s2)

empty :: (Ord a) => Set a
empty = Prim S.empty

singleton :: (Ord a) => a -> Set a
singleton a = Prim (S.singleton a)

insert :: (Ord a) => a -> Set a -> Set a
insert a s = Prim (S.insert a (run s))

delete :: (Ord a) => a -> Set a -> Set a
delete a s = Prim (S.delete a (run s))

union :: (Ord a) => Set a -> Set a -> Set a
union s1 s2 = Prim (run s1 `S.union` run s2)

unions :: (Ord a) => [Set a] -> Set a
unions ss = Prim (S.unions (L.map run ss))

difference :: (Ord a) => Set a -> Set a -> Set a
difference s1 s2 = Prim (S.difference (run s1) (run s2))

intersection :: (Ord a) => Set a -> Set a -> Set a
intersection s1 s2 = Prim (S.intersection (run s1) (run s2))

filter :: (Ord a) => (a -> Bool) -> Set a -> Set a
filter f s = Prim (S.filter f (run s))

partition :: (Ord a) => (a -> Bool) -> Set a -> (Set a,Set a)
partition f s = (Prim *** Prim) (S.partition f (run s))

split :: (Ord a) => a -> Set a -> (Set a,Set a)
split a s = (Prim *** Prim) (S.split a (run s))

splitMember :: (Ord a) => a -> Set a -> (Set a, Bool, Set a)
splitMember a s = (\(s1,b,s2) -> (Prim s1,b,Prim s2)) (S.splitMember a (run s))

map :: (Ord a,Ord b) => (a -> b) -> Set a -> Set b
map f s = Prim (S.map f (run s))

mapMonotonic :: (Ord a,Ord b) => (a -> b) -> Set a -> Set b
mapMonotonic f s = Prim (S.mapMonotonic f (run s))

foldr :: (Ord a) => (a -> b -> b) -> b -> Set a -> b
foldr f z s = S.foldr f z (run s)

foldl :: (Ord a) => (b -> a -> b) -> b -> Set a -> b
foldl f z s = S.foldl f z (run s)

foldr' :: (Ord a) => (a -> b -> b) -> b -> Set a -> b
foldr' f z s = S.foldr' f z (run s)

foldl' :: (Ord a) => (b -> a -> b) -> b -> Set a -> b
foldl' f z s = S.foldl' f z (run s)

fold :: (Ord a) => (a -> b -> b) -> b -> Set a -> b
fold = foldr

findMin :: (Ord a) => Set a -> a
findMin = S.findMin . run

findMax :: (Ord a) => Set a -> a
findMax = S.findMax . run

deleteMin :: (Ord a) => Set a -> Set a
deleteMin = Prim . S.deleteMin . run

deleteMax :: (Ord a) => Set a -> Set a
deleteMax = Prim . S.deleteMax . run

deleteFindMin :: (Ord a) => Set a -> (a,Set a)
deleteFindMin s = second Prim (S.deleteFindMin (run s))

deleteFindMax :: (Ord a) => Set a -> (a,Set a)
deleteFindMax s = second Prim (S.deleteFindMax (run s))

maxView :: (Ord a) => Set a -> Maybe (a,Set a)
maxView = fmap (second Prim) . S.maxView . run

minView :: (Ord a) => Set a -> Maybe (a,Set a)
minView = fmap (second Prim) . S.minView . run

elems :: (Ord a) => Set a -> [a]
elems = toList

toList :: (Ord a) => Set a -> [a]
toList = S.toList . run

fromList :: (Ord a) => [a] -> Set a
fromList as = Prim (S.fromList as)

toAscList :: (Ord a) => Set a -> [a]
toAscList = S.toAscList . run

fromAscList :: (Ord a) => [a] -> Set a
fromAscList = Prim . S.fromAscList

fromDistinctAscList :: (Ord a) => [a] -> Set a
fromDistinctAscList = Prim . S.fromDistinctAscList

showTree :: (Show a,Ord a) => Set a -> String
showTree = S.showTree . run

showTreeWith :: (Show a, Ord a) => Bool -> Bool -> Set a -> String
showTreeWith b1 b2 s = S.showTreeWith b1 b2 (run s)

valid :: (Ord a) => Set a -> Bool
valid = S.valid . run

