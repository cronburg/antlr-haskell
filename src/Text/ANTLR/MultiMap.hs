{-# LANGUAGE DeriveGeneric, DeriveAnyClass, MonadComprehensions, DeriveLift,
      DeriveDataTypeable #-}
{-|
  Module      : Text.ANTLR.MultiMap
  Description : A one-to-many key value map
  Copyright   : (c) Karl Cronburg, 2018
  License     : BSD3
  Maintainer  : karl@cs.tufts.edu
  Stability   : experimental
  Portability : POSIX

-}
module Text.ANTLR.MultiMap where
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Text.ANTLR.Set (Generic(..), Hashable(..), Set(..))
import qualified Text.ANTLR.Set as S
import Prelude hiding (lookup)
import Text.ANTLR.Pretty

import Data.Data (Data(..))
import Language.Haskell.TH.Syntax (Lift(..))

instance (Lift k, Lift v, Data k, Data v, Ord k, Ord v) => Lift (M.Map k v)

-- | A multi 'Map' is a mapping from keys @k@ to sets of values @v@. A nice
--   invariant to maintain while using a multi-map is to never have empty
--   sets mapped to by some key.
newtype Map k v = Map (M.Map k (Set v))
  deriving (Generic, Hashable, Eq, Show, Lift)

instance (Prettify k, Prettify v, Hashable v, Eq v) => Prettify (Map k v) where
  prettify (Map m) = prettify m

-- | The singleton multimap, given a single key and a __single__ value.
singleton :: (Hashable v, Eq v) => k -> v -> Map k v
singleton k v = Map (M.singleton k (S.singleton v))

-- | Construct a multi 'Map' from a list of key-value pairs.
fromList :: (Hashable v, Ord k, Eq k, Eq v) => [(k, v)] -> Map k v
fromList kvs = Map (M.fromList
  [ (k1, S.fromList [v2 | (k2, v2) <- kvs, k1 == k2])
  | (k1, _) <- kvs])

-- | Same as 'fromList' but where the values in the key-value tuples are already in sets.
fromList' :: (Ord k, Eq k, Hashable v, Eq v) => [(k, Set v)] -> Map k v
fromList' kvs = fromList [(k, v) | (k, vs) <- kvs, v <- S.toList vs]

-- | Inverse of 'fromList\''.
toList :: Map k v -> [(k, Set v)]
toList (Map m) = M.toList m

-- | Take the union of two maps.
union :: (Ord k, Eq k, Hashable v, Eq v) => Map k v -> Map k v -> Map k v
union m1 m2 = fromList' (toList m1 ++ toList m2)

-- | The empty multi-map.
empty :: Map k v
empty = Map M.empty

-- | Get the set of values mapped to by some key @k@.
lookup :: (Ord k, Hashable v, Eq v) => k -> Map k v -> Set v
lookup k (Map m) = fromMaybe S.empty (M.lookup k m)

-- | Number of keys in the multi-map.
size (Map m) = M.size m

-- | Map difference of two multi-maps, deleting individual key-value pairs
--   rather than deleting the entire key. Invariant maintained is that
--   input maps with non-null value sets will result in an output with
--   non-null value sets.
difference (Map m1) m2 = Map $ M.fromList
  [ (k1, vs)
  | (k1, vs1) <- M.toList m1
  , let vs2 = k1 `lookup` m2
  , let vs  = vs1 `S.difference` vs2
  , (not . S.null) vs
  ]

