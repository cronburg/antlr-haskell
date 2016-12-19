{-# LANGUAGE ScopedTypeVariables #-}
module Text.ANTLR.LR1
  ( Item(..)
  , closure, slrGoto, slrItems
  , ItemLHS(..)
  , kernel, allItems, items
  , slrTable, Token(..), Action(..), LRState, LRTable
  , lrParse, slrParse
  ) where
import Text.ANTLR.Allstar.Grammar
import qualified Text.ANTLR.LL1 as LL
import Text.ANTLR.LL1 (Token(..))

import Data.Set ( Set(..), fromList, empty, member, toList, size
  , union, (\\), insert, toList, singleton
  )
import qualified Data.Set as S
import Data.Map ( Map(..) )
import qualified Data.Map as M

import System.IO.Unsafe (unsafePerformIO)
uPIO = unsafePerformIO

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

type Goto = Set Item -> ProdElem -> Set Item

slrGoto :: Grammar () -> Goto
slrGoto g is _X = closure g $ fromList
  [ Item _A (_X : α) β
  | Item _A α (_X' : β) <- toList is
  , _X == _X'
  ]

items :: Grammar () -> Goto -> Set (Set Item)
items g goto = let
    items' :: Set (Set Item) -> Set (Set Item)
    items' _C = let
      add = fromList
            [ goto is _X
            | is <- toList _C
            , _X <- toList $ symbols g
            , not . null $ goto is _X
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

type LRState  = Set Item
type LRTable = Map (LRState, Token) Action

data Action =
    Shift   LRState
  | Reduce (Production ())
  | Accept
  | Error
  deriving (Eq, Ord, Show)

slrItems :: Grammar () -> Set (Set Item)
slrItems g = items g $ slrGoto g

slrTable :: Grammar () -> LRTable
slrTable g = let

    --slr' :: a -> b -> b
    --slr' :: Set Item -> Item -> LRTable -> LRTable
    slr' :: Set Item -> LRTable
    slr' _Ii = let
        slr'' :: Item -> LRTable
        slr'' (Item (ItemNT nt) α (T a:β)) = M.singleton (_Ii, Token a) (Shift $ slrGoto g _Ii $ NT a)
        slr'' (Item (Init   nt) α (T a:β)) = M.singleton (_Ii, Token a) (Shift $ slrGoto g _Ii $ NT a)
        slr'' (Item (ItemNT nt) α [])      = M.fromList
                                          [ ((_Ii, a), Reduce (nt, Prod α))
                                          | a <- (toList . LL.follow g) nt
                                          ]
        slr'' (Item (Init nt) α [])   = M.singleton (_Ii, LL.EOF) Accept
        slr'' _ = M.empty
      in S.fold M.union M.empty (S.map slr'' _Ii)

  in S.fold M.union M.empty $ S.map slr' $ slrItems g

type Config = ([LRState], [Token])

look :: (LRState, Token) -> LRTable -> Maybe Action
look (s,a) act = uPIO (print ("lookup:", s, a, M.lookup (s, a) act)) `seq` M.lookup (s, a) act

lrParse :: Grammar () -> LRTable -> Goto -> [Token] -> Bool
lrParse g act goto w = let
  
    lr :: Config -> Bool
    lr (s:states, a:ws) = let
        
        lr' :: Maybe Action -> Bool
        lr' Nothing = False
        lr' (Just Accept) = True
        lr' (Just Error)  = False
        lr' (Just (Shift t)) = lr (t:s:states, ws)
        lr' (Just (Reduce (_A, Prod β))) = let
              ss'@(t:_) = drop (length β) (s:states)
            in uPIO (print ("Reduce:", _A, Prod β)) `seq` lr (goto t (NT _A) : ss', a:ws)

      in lr' $ look (s,a) act

    s_0 = Item (Init $ s0 g) [] [NT $ s0 g]

  in lr ([closure g $ S.singleton s_0], w)

slrParse :: Grammar () -> [Token] -> Bool
slrParse g = lrParse g (slrTable g) (slrGoto g)

