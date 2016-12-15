{-# LANGUAGE ScopedTypeVariables #-}
module Text.ANTLR.LR1
  ( Item(..)
  , closure
  , ItemLHS(..)
  , kernel, allItems
  ) where
import Text.ANTLR.Allstar.Grammar
import Data.Set ( Set(..), fromList, empty, member, toList, size
  , union, (\\), insert, toList
  )
import qualified Data.Set as S

data ItemLHS =
    Init   NonTerminal -- S' if S is the grammar start symbol
  | ItemNT NonTerminal -- wrapper around a NonTerminal
  deriving (Eq, Ord, Show)

-- An Item is a production with a dot in it indicating how far
-- into the production we have parsed:
--               A       ->  α          .    β
data Item = Item ItemLHS Symbols {- . -} Symbols
  deriving (Eq, Ord, Show)

closure :: Grammar () -> Set Item -> Set Item
closure g is' = let

    closure' :: Set Item -> Set Item
    closure' _J = let
      add = fromList
            [ Item (ItemNT _B) [] γ
            | Item _A α rst@(pe@(NT _B) : β) <- toList _J
            , not $ null rst
            , isNT pe
            , (_, p@(Prod γ)) <- prodsFor g _B
            , isProd p
            ]
      in case size $ add \\ _J of
        0 -> _J `union` add
        _ -> closure' $ _J `union` add

  in closure' is'

kernel :: Set Item -> Set Item
kernel = let
    kernel' (Item (Init   _) _  _) = True
    kernel' (Item (ItemNT _) [] _) = False
    kernel' _ = True
  in S.filter kernel'

-- Generate the set of all possible Items for a given grammar:
allItems :: Grammar () -> Set Item
allItems g = insert (Item (Init $ s0 g) [] [NT $ s0 g]) $ fromList
  [ Item (ItemNT nt) (take n γ) (drop n γ)
  | nt <- toList $ ns g
  , (_, p@(Prod γ)) <- prodsFor g nt
  , isProd p
  , n <- [0..length γ]
  ]

