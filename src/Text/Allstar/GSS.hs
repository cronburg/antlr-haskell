module Text.Allstar.GSS where
import Prelude hiding (map)
import qualified Data.Set (Set, union, empty, foldr, map, filter)
-- Graph Structured Stack

--newtype Edge a = Edge (Node a, Node a)

--newtype GSS a = Graph (Set (Edge a))

-- I lied. This isn't graph structured (yet).
newtype GSS a = Set [a]

push :: a -> GSS a -> GSS a
push a = map ((:) a)

pop :: GSS a -> Set (a, GSS a)
pop gss = let
  
  pop' [] gss' = gss'
  pop' (a:as) gss'
    | (null . snd . partition (a ==) . head) gss' = gss'
    | otherwise = insert (a, insert as $ delete (a:as) gss) gss'
  in foldr pop' empty gss

-- Merge:
--(\+/) = union
--(#)   = Wildcard

-- configuration
--type Configuration = (ATNState, Int, Gamma)

