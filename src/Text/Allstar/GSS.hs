{-# LANGUAGE ScopedTypeVariables #-}
module Text.Allstar.GSS
  ( GSS, Edge
  , pop, push, topPartition, merge
  , toStacks, fromStacks
  ) where
import Prelude hiding (map, foldr, null, filter)
import Data.Set ( Set, union, empty, foldr, map, filter
                , partition, insert, delete, null, fromList
                , member, toList, unions)

-- USE FGL?

fL :: Ord a => [a] -> Set a
fL = fromList

tL :: Ord a => Set a -> [a]
tL = toList

type GSS a = (Set (Edge a), Set a)

type Edge a = (a, a)

pop :: Ord a => GSS a -> GSS a
pop  (g, t) = (g, fL [σ' | (σ', σ) <- tL g, σ `member` t])

push :: Ord a => GSS a -> a -> GSS a
push (g, t) σ = (g `union` fL [(σ', σ) | σ' <- tL t], fL [σ])

topPartition :: Ord a => GSS a -> Set (GSS a)
topPartition (g, t) = fL [(g, fL [σ]) | σ <- tL t]

merge :: Ord a => GSS a -> GSS a -> GSS a
merge (g1,t1) (g2,t2) = (g1 `union` g2, t1 `union` t2)

fromStacks :: Ord a => Set [a] -> GSS a
fromStacks ss = let

  -- Add the top node of each nonempty stacks into a set of nodes:
  tops :: Ord a => [a] -> Set a -> Set a
  tops [] vs     = vs
  tops (a:as) vs = insert a vs

  -- Recursively construct the set of edges for a GSS from a given linear stack:
  graphs :: Ord a => [a] -> Set (Edge a) -> Set (Edge a)
  graphs []       es  = es
  graphs [a]      es  = es
  graphs [a,b]    es  = insert (b,a) es
  graphs (a:b:as) es  = insert (b,a) (graphs (b:as) es)

  in (foldr graphs empty ss, foldr tops empty ss)

toStacks :: Ord a => GSS a -> Set [a]
toStacks gss = let

  toStacks' :: Ord a => GSS a -> Set [a]
  toStacks' gss@(es, vs) = fL
    [v:vs' | v <- tL vs, vs' <- (tL . toStacks' . pop) gss, v `member` (snd.pop) gss]

  in toStacks' gss

-- Is there a path from vertex a to b in g?
path :: Ord a => GSS a -> a -> a -> Bool
path gss@(g, _) = let
    path' a b
      | a == b    = True
      | null g    = False
      | otherwise = or [path' b' b | (a', b') <- tL g, a' == a]
  in path'

-- Prune unnecessary edges in the edge list - unreachable edges past the heads
-- of the stack(s). Mostly useful for debugging what we expect the GSS to look
-- like even though the graph structured stacks are functionally equivalent.
prune :: Ord a => GSS a -> GSS a
prune gss@(g, t) = (foldl union empty [filter (\(a, _) -> path gss a b) g | b <- tL t], t)

