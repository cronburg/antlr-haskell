{-# LANGUAGE ScopedTypeVariables #-}
module Text.ANTLR.Allstar.Stacks
  ( Stacks(..)
  ) where
import qualified Prelude as P
import Prelude hiding (map, foldr, filter)
import Data.Set (union, Set(..), foldr, map, filter, fromList, singleton)
import qualified Data.Set as Set
import Data.List (nub)

data Stacks a =
    Empty
  | Wildcard
  | Stacks (Set [a])

(#) = Wildcard

merge :: Ord a => Stacks a -> Stacks a -> Stacks a
merge Wildcard _   = Wildcard
merge _ Wildcard   = Wildcard
merge Empty Empty  = Empty
merge (Stacks _Γ)  Empty       = Stacks $ _Γ `union` fromList [[]]
merge Empty       (Stacks _Γ)  = Stacks $ _Γ `union` fromList [[]]
merge (Stacks _Γ) (Stacks _Γ') = Stacks $ _Γ `union` _Γ'

push :: Ord a => a -> Stacks a -> Stacks a
push a Empty        = Stacks $ singleton [a]
push a Wildcard     = Wildcard
push a (Stacks _Γ)  = Stacks $ map ((:) a) _Γ

-- Get heads of non-empty stacks / lists:
heads :: Set [a] -> [a]
heads = let
    heads' :: [a] -> [a] -> [a]
    heads' []     bs = bs
    heads' (a:as) bs = a:bs
  in foldr heads' []

-- TODO: pop on Empty or Wildcard?
pop  :: Ord a => Stacks a -> [(a, Stacks a)]
pop Empty = []
pop Wildcard = []
pop (Stacks _Γ) = let
--    ss :: a -> Stacks a
    ss a = Stacks $ map tail $ filter (\as -> (not . null) as && ((== a) . head) as) _Γ
  in P.map (\a -> (a, ss a)) (nub . heads $ _Γ)

