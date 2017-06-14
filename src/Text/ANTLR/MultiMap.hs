{-# LANGUAGE DeriveGeneric, DeriveAnyClass, MonadComprehensions #-}
module Text.ANTLR.MultiMap where
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Text.ANTLR.Set (Generic(..), Hashable(..), Set(..))
import qualified Text.ANTLR.Set as S
import Prelude hiding (lookup)
import Text.ANTLR.Pretty

newtype Map k v = Map (M.Map k (Set v))
  deriving (Generic, Hashable, Eq)

instance (Prettify k, Prettify v, Hashable v, Eq v) => Prettify (Map k v) where
  prettify (Map m) = prettify m

singleton :: (Hashable v, Eq v) => k -> v -> Map k v
singleton k v = Map (M.singleton k (S.singleton v))

fromList :: (Hashable v, Ord k, Eq k, Eq v) => [(k, v)] -> Map k v
fromList kvs = Map (M.fromList
  [ (k1, S.fromList [v2 | (k2, v2) <- kvs, k1 == k2])
  | (k1, _) <- kvs])

fromList' :: (Ord k, Eq k, Hashable v, Eq v) => [(k, Set v)] -> Map k v
fromList' kvs = fromList [(k, v) | (k, vs) <- kvs, v <- S.toList vs]

toList :: Map k v -> [(k, Set v)]
toList (Map m) = M.toList m

union :: (Ord k, Eq k, Hashable v, Eq v) => Map k v -> Map k v -> Map k v
union m1 m2 = fromList' (toList m1 ++ toList m2)

empty :: Map k v
empty = Map M.empty

lookup :: (Ord k, Hashable v, Eq v) => k -> Map k v -> Set v
lookup k (Map m) = fromMaybe S.empty (M.lookup k m)

size (Map m) = M.size m

difference (Map m1) m2 = Map $ M.fromList
  [ (k1, vs)
  | (k1, vs1) <- M.toList m1
  , let vs2 = k1 `lookup` m2
  , let vs  = vs1 `S.difference` vs2
  , (not . S.null) vs
  ]

