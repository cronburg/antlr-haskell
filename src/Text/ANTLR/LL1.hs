module Text.ANTLR.LL1
  ( recognize
  , first, follow
  , Token(..)
  , foldWhileEpsilon
  ) where
import Text.ANTLR.Allstar.Grammar
import Text.ANTLR.Allstar.ATN
import Data.Set ( Set(..), singleton, fromList, union, empty, member, size, toList
                , insert, delete
                )

-- An LL1 token (as used in first and follow sets) is either a
-- terminal in the grammar's alphabet, or an epsilon
data Token =
    Term Terminal
  | Eps'
  | EOF -- End of input really, but EOF is ubiquitous.
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

first :: Grammar () -> [ProdElem] -> Set Token
first g = let
    firstOne :: Set ProdElem -> ProdElem -> Set Token
    firstOne _ t@(T x) = singleton $ Term x
    firstOne _ Eps     = singleton Eps'
    firstOne busy nt@(NT x)
      | nt `member` busy = empty
      | otherwise = foldr union empty
            [ foldWhileEpsilon union empty
              [ firstOne (insert nt busy) y
              | y <- (\(Prod ss) -> ss) rhs
              ]
            | (_, rhs) <- prodsFor g x
            , isProd rhs
            ]
    
    firstMany :: [Set Token] -> Set Token
    --firstMany :: Set Token -> Set Token -> Set Token
    firstMany []   = singleton Eps'
    firstMany (ts:tss)
      | Eps' `member` ts = ts `union` firstMany tss
      | otherwise        = ts
  in firstMany . map (firstOne empty)

follow :: Grammar () -> NonTerminal -> Set Token
follow g = let
    follow' busy _B
      | _B `member` busy = empty
      | otherwise = let

        busy' = insert _B busy
        
        followProd :: NonTerminal -> Symbols -> Set Token
        followProd _  []  = empty
        followProd _A [s]
              -- If A -> αB then everything in FOLLOW(A) is in FOLLOW(B)
          | s == NT _B = follow' busy' _A
          | otherwise  = empty
        followProd _A (s:β)
              -- Recursively find all other instances of B in this production
          | s /= NT _B = followProd _A β
          | otherwise  =
              -- Recursively find all other instances of B in this production
              followProd _A β
              `union`
              -- If A -> αBβ, then everything in FIRST(β) is in FOLLOW(B)
              (delete Eps' $ first g β)
              `union`
              -- If A -> αBβ and Epsilon `member` FIRST(β), then everything
              -- in FOLLOW(A) is in FOLLOW(B)
              (if Eps' `member` first g β
                then follow' busy' _A
                else empty
              )

            -- Start state contains EOF (aka '$', end of input) in FOLLOW()
      in  (if _B == s0 g then singleton EOF else empty)
          `union`
          foldr union empty [ followProd lhs_nt ss
                            | (lhs_nt, Prod ss) <- filter (isProd . snd) . ps $ g
                            ]
  in follow' empty

