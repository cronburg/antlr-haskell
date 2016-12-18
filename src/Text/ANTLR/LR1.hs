{-# LANGUAGE ScopedTypeVariables #-}
module Text.ANTLR.LR1
  ( Item(..)
  , closure, goto
  , ItemLHS(..)
  , kernel, allItems, items
  ) where
import Text.ANTLR.Allstar.Grammar
import Data.Set ( Set(..), fromList, empty, member, toList, size
  , union, (\\), insert, toList, singleton
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

goto :: Grammar () -> Set Item -> ProdElem -> Set Item
goto g is _X = closure g $ fromList
  [ Item _A (_X : α) β
  | Item _A α (_X' : β) <- toList is
  , _X == _X'
  ]

items :: Grammar () -> Set (Set Item)
items g = let
    items' :: Set (Set Item) -> Set (Set Item)
    items' _C = let
      add = fromList
            [ goto g is _X
            | is <- toList _C
            , _X <- toList $ symbols g
            , not . null $ goto g is _X
            ]
      in case size $ add \\ _C of
        0 -> _C `union` add
        _ -> items' $ _C `union` add
  in items' $ singleton $ closure g $ singleton (Item (Init $ s0 g) [] [NT $ s0 g])

kernel :: Set Item -> Set Item
kernel = let
    kernel' (Item (Init   _) _  _) = True
    kernel' (Item (ItemNT _) [] _) = False
    kernel' _ = True
  in S.filter kernel'

-- Generate the set of all possible Items for a given grammar:
allItems :: Grammar () -> Set Item
allItems g = fromList
    [ Item (Init $ s0 g) [] [NT $ s0 g]
    , Item (Init $ s0 g) [NT $ s0 g] []
    ]
  `union`
  fromList
    [ Item (ItemNT nt) (reverse $ take n γ) (drop n γ)
    | nt <- toList $ ns g
    , (_, p@(Prod γ)) <- prodsFor g nt
    , isProd p
    , n <- [0..length γ]
    ]

