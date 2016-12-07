module Text.ANTLR.LL1
  ( recognize
  , first
  , Token(..)
  , foldWhileEpsilon
  ) where
import Text.ANTLR.Allstar.Grammar
import Text.ANTLR.Allstar.ATN
import Data.Set ( Set(..), singleton, fromList, union, empty, member, size, toList
                , insert
                )

-- An LL1 token (as used in first and follow sets) is either a
-- terminal in the grammar's alphabet, or an epsilon
data Token =
    Term Terminal
  | Eps'
  deriving (Eq, Ord, Show)

recognize :: Grammar () -> [Token] -> Bool
recognize g ts = True

-- Fold while the given pred function is true:
foldWhile :: (a -> b -> Bool) -> (a -> b -> b) -> b -> [a] -> b
foldWhile pred fncn = let
    fW' b0 []     = b0
    fW' b0 [a]    = b0
    fW' b0 (a:as)
      | pred a b0 = fW' (fncn a b0) as
      | otherwise = b0
  in fW'

epsIn set _ = Eps' `member` set

-- Fold over a set of Tokens while all the previous sets of
-- tokens contains an epsilon.
foldWhileEpsilon fncn b0 []     = empty
foldWhileEpsilon fncn b0 [a]    = fncn a b0
foldWhileEpsilon fncn b0 (a:as)
  | epsIn a b0 = foldWhile epsIn fncn (fncn a b0) as
  | otherwise  = fncn a b0

first :: Grammar () -> ProdElem -> Set Token
first g = let
    first' :: Set ProdElem -> ProdElem -> Set Token
    first' _ t@(T x) = singleton $ Term x
    first' _ Eps     = singleton Eps'
    first' busy nt@(NT x)
      | nt `member` busy = empty
      | otherwise = foldr union empty
            [ foldWhileEpsilon union empty
              [ first' (insert nt busy) y
              | y <- (\(Prod ss) -> ss) rhs
              ]
            | (_, rhs) <- prodsFor g x
            , isProd rhs
            ]

{-
              [ first' (insert y busy) y
              | (_, rhs) <- prodsFor g x
              , isProd rhs
              --, (y, i) <- zipWith (\(Prod ss) i -> (ss,i)) prod [1..]
              , y <- (\(Prod ss) -> ss) rhs
              ]
-}
  in first' empty

