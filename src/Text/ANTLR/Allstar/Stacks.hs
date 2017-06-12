{-# LANGUAGE ScopedTypeVariables, DeriveAnyClass, DeriveGeneric,
    OverloadedStrings #-}
module Text.ANTLR.Allstar.Stacks
  ( Stacks(..)
  , (#)
  , merge
  , push
  , pop
  ) where
import qualified Prelude as P
import Prelude hiding (map, foldr, filter)
import Text.ANTLR.Set
  ( union, Set(..), foldr, map, filter
  , fromList, singleton, Hashable(..), Generic(..)
  )
import qualified Text.ANTLR.Set as Set
import Data.List (nub)
import Text.ANTLR.Pretty

data Stacks a =
    Empty
  | Wildcard
  | Stacks (Set [a])
  deriving (Eq, Ord, Generic, Hashable, Show)

instance (Prettify a, Hashable a, Eq a) => Prettify (Stacks a) where
  prettify Empty      = pStr "[]"
  prettify Wildcard   = pStr "#"
  prettify (Stacks s) = prettify s

(#) = Wildcard

merge :: (Eq a, Hashable a) => Stacks a -> Stacks a -> Stacks a
merge Wildcard _   = Wildcard
merge _ Wildcard   = Wildcard
merge Empty Empty  = Empty
merge (Stacks _Γ)  Empty       = Stacks $ _Γ `union` fromList [[]]
merge Empty       (Stacks _Γ)  = Stacks $ _Γ `union` fromList [[]]
merge (Stacks _Γ) (Stacks _Γ') = Stacks $ _Γ `union` _Γ'

push :: (Eq a, Hashable a) => a -> Stacks a -> Stacks a
push a Empty        = Stacks $ singleton [a]
push a Wildcard     = Wildcard
push a (Stacks _Γ)  = Stacks $ map ((:) a) _Γ

-- Get heads of non-empty stacks / lists:
heads :: (Eq a, Hashable a) => Set [a] -> [a]
heads = let
    heads' :: [a] -> [a] -> [a]
    heads' []     bs = bs
    heads' (a:as) bs = a:bs
  in foldr heads' []


-- TODO: pop on Empty or Wildcard?
pop  :: (Eq a, Hashable a) => Stacks a -> [(a, Stacks a)]
pop Empty = []
pop Wildcard = []
pop (Stacks _Γ) = let
--    ss :: a -> Stacks a
    ss a = Stacks $ map tail $ filter (\as -> (not . null) as && ((== a) . head) as) _Γ
  in P.map (\a -> (a, ss a)) (nub . heads $ _Γ)
