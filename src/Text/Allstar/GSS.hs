{-# LANGUAGE ScopedTypeVariables #-}
module Text.Allstar.GSS where
import Prelude hiding (map, foldr, null)
import Data.Set ( Set, union, empty, foldr, map, filter
                , partition, insert, delete, null, fromList)
-- Graph Structured Stack

--newtype Edge a = Edge (Node a, Node a)

--newtype GSS a = Graph (Set (Edge a))

-- I lied. This isn't graph structured (yet).
type GSS a = Set [a]

push :: Ord a => a -> GSS a -> GSS a
push a = map ((:) a)

pop :: Ord a => GSS a -> Set (a, GSS a)
pop gss = let

--  pop' :: [a] -> Set (a, GSS a) -> Set (a, GSS a)
  pop' [] gss' = gss'
  pop' (a:as) gss'
    | (not . null . fst . partition ((a ==) . fst)) gss' = gss'
    | otherwise = insert (a, insert as $ delete (a:as) gss) gss'

  in foldr pop' empty gss

-- Merge:
--(\+/) = union
--(#)   = Wildcard

-- configuration
--type Configuration = (ATNState, Int, Gamma)

