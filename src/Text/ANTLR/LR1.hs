{-# LANGUAGE ScopedTypeVariables, ExplicitForAll #-}
module Text.ANTLR.LR1
  ( Token(..), Action, Item(..), ItemLHS(..), ParseEvent(..)
  , kernel, items
  , slrClosure, slrGoto, slrItems, allSLRItems, slrTable, slrParse, slrRecognize
  , lr1Closure, lr1Goto, lr1Items, lr1Table, lr1Parse, lr1Recognize
  , LRState, LRTable, LRAction(..)
  , lrParse
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
data Item a = Item ItemLHS Symbols {- . -} Symbols a
  deriving (Eq, Ord, Show)

type Closure a = Set (Item a) -> Set (Item a)

type SLRClosure = Closure ()
type LR1Closure = Closure LR1LookAhead

slrClosure :: Grammar () -> SLRClosure
slrClosure g is' = let

    closure' :: SLRClosure
    closure' _J = let
      add = fromList
            [ Item (ItemNT _B) [] γ ()
            | Item _A α rst@(pe@(NT _B) : β) () <- toList _J
            , not $ null rst
            , isNT pe
            , (_, p@(Prod γ)) <- prodsFor g _B
            , isProd p
            ]
      in case size $ add \\ _J of
        0 -> _J `union` add
        _ -> closure' $ _J `union` add

  in closure' is'

lr1Closure :: Grammar () -> Closure LR1LookAhead
lr1Closure g is' = let

    tokenToProdElem (Token a) = [T a]
    tokenToProdElem _ = []

    closure' :: LR1Closure
    closure' _J = let
      add = fromList
            -- TODO: Handle EOF in LL.first set calculation properly?:
            [ Item (ItemNT _B) [] γ (if b == Eps' then EOF else b)
            | Item _A α rst@(pe@(NT _B) : β) a <- toList _J
            , not $ null rst
            , isNT pe
            , (_, p@(Prod γ)) <- prodsFor g _B
            , isProd p
            , b <- toList $ LL.first g (β ++ tokenToProdElem a)
            ]
      in case size $ add \\ _J of
        0 -> _J `union` add
        _ -> closure' $ _J `union` add

  in closure' is'

type Goto a = LRState a -> ProdElem -> LRState a

goto :: Ord a => Grammar () -> Closure a -> Goto a
goto g closure is _X = closure $ fromList
  [ Item _A (_X : α) β  a
  | Item _A α (_X' : β) a <- toList is
  , _X == _X'
  ]

slrGoto :: Grammar () -> Goto ()
slrGoto g = goto g (slrClosure g)

items :: forall a. Ord a => Grammar () -> Goto a -> Closure a -> LRState a -> Set (LRState a)
items g goto closure s0 = let
    items' :: Set (LRState a) -> Set (LRState a)
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
  in items' $ singleton $ closure s0
--  singleton (Item (Init $ s0 g) [] [NT $ s0 g])

kernel :: Set (Item a) -> Set (Item a)
kernel = let
    kernel' (Item (Init   _) _  _ _) = True
    kernel' (Item (ItemNT _) [] _ _) = False
    kernel' _ = True
  in S.filter kernel'

-- Generate the set of all possible Items for a given grammar:
allSLRItems :: Grammar () -> Set SLRItem
allSLRItems g = fromList
    [ Item (Init $ s0 g) [] [NT $ s0 g] ()
    , Item (Init $ s0 g) [NT $ s0 g] [] ()
    ]
  `union`
  fromList
    [ Item (ItemNT nt) (reverse $ take n γ) (drop n γ) ()
    | nt <- toList $ ns g
    , (_, p@(Prod γ)) <- prodsFor g nt
    , isProd p
    , n <- [0..length γ]
    ]

type LRState a = Set (Item a)
type LRTable a = Map (LRState a, Token) (LRAction a)

data LRAction a =
    Shift  (LRState a)
  | Reduce (Production ())
  | Accept
  | Error
  deriving (Eq, Ord, Show)

type SLRItem = Item ()
type SLRState = LRState ()
type SLRTable = LRTable ()

lrS0 :: a -> Grammar () -> LRState a
lrS0 a g = singleton $ Item (Init $ s0 g) [] [NT $ s0 g] a

slrS0 :: Grammar () -> SLRState
slrS0 = lrS0 ()

slrItems :: Grammar () -> Set (Set SLRItem)
slrItems g = items g (slrGoto g) (slrClosure g) (slrS0 g)

slrTable :: Grammar () -> SLRTable
slrTable g = let

    --slr' :: a -> b -> b
    --slr' :: Set Item -> Item -> LRTable -> LRTable
    slr' :: SLRState -> SLRTable
    slr' _Ii = let
        slr'' :: SLRItem -> SLRTable
        slr'' (Item (ItemNT nt) α (T a:β) ()) = --uPIO (print ("TABLE:", a, slrGoto g _Ii $ T a, _Ii)) `seq`
                  M.singleton (_Ii, Token a) (Shift $ slrGoto g _Ii $ T a)
        slr'' (Item (Init   nt) α (T a:β) ()) = M.singleton (_Ii, Token a) (Shift $ slrGoto g _Ii $ T a)
        slr'' (Item (ItemNT nt) α [] ())      = M.fromList
                                          [ ((_Ii, a), Reduce (nt, Prod $ reverse α))
                                          | a <- (toList . LL.follow g) nt
                                          ]
        slr'' (Item (Init nt) α [] ())   = M.singleton (_Ii, LL.EOF) Accept
        slr'' _ = M.empty
      in S.fold M.union M.empty (S.map slr'' _Ii)

  in S.fold M.union M.empty $ S.map slr' $ slrItems g

type LR1Item  = Item    LR1LookAhead
type LR1Table = LRTable LR1LookAhead

lr1Table :: Grammar () -> LR1Table
lr1Table g = let
    lr1' :: LR1State -> LR1Table
    lr1' _Ii = let
        lr1'' :: LR1Item -> LR1Table
        lr1'' (Item (ItemNT nt) α (T a:β) _) = --uPIO (print ("TABLE:", a, slrGoto g _Ii $ T a, _Ii)) `seq`
                  M.singleton (_Ii, Token a) (Shift $ lr1Goto g _Ii $ T a)
        lr1'' (Item (Init   nt) α (T a:β) _) = M.singleton (_Ii, Token a) (Shift $ lr1Goto g _Ii $ T a)
        lr1'' (Item (ItemNT nt) α [] a)      = M.singleton (_Ii,       a) (Reduce (nt, Prod $ reverse α))
        lr1'' (Item (Init nt) α [] EOF)   = M.singleton (_Ii, LL.EOF) Accept
        lr1'' _ = M.empty
      in S.fold M.union M.empty (S.map lr1'' _Ii)

  in S.fold M.union M.empty $ S.map lr1' $ lr1Items g

type Config a = ([LRState a], [Token])

look :: Ord a => (LRState a, Token) -> LRTable a -> Maybe (LRAction a)
look (s,a) tbl = --uPIO (print ("lookup:", s, a, M.lookup (s, a) act)) `seq`
    M.lookup (s, a) tbl

-- TODO: unify with LL
data ParseEvent ast =
    TokenE Token
  | NonTE  (NonTerminal, Symbols, [ast])

type Action ast = ParseEvent ast -> ast

lrParse :: forall ast a. Ord a => Grammar () -> LRTable a -> Goto a -> Closure a -> LRState a -> Action ast -> [Token] -> Maybe ast
lrParse g tbl goto closure s_0 act w = let
  
    lr :: Config a -> [ast] -> Maybe ast
    lr (s:states, a:ws) asts = let
        
        lr' :: Maybe (LRAction a) -> Maybe ast
        lr' Nothing          = Nothing
        lr' (Just Accept)    = case length asts of
              1 -> Just $ head asts
              _ -> Nothing
        lr' (Just Error)     = Nothing
        lr' (Just (Shift t)) = lr (t:s:states, ws) $ act (TokenE a) : asts
        lr' (Just (Reduce (_A, Prod β))) = let
              ss'@(t:_) = drop (length β) (s:states)
            in lr (goto t (NT _A) : ss', a:ws)
                  ((act $ NonTE (_A, β, reverse $ take (length β) asts)) : drop (length β) asts)

      in lr' $ look (s,a) tbl

  in lr ([closure s_0], w) []

slrParse :: Grammar () -> Action ast -> [Token] -> Maybe ast
slrParse g = lrParse g (slrTable g) (slrGoto g) (slrClosure g) (slrS0 g)

slrRecognize :: Grammar () -> [Token] -> Bool
slrRecognize g w = (Nothing /=) $ slrParse g (const 0) w

lr1Recognize :: Grammar () -> [Token] -> Bool
lr1Recognize g w = (Nothing /=) $ lr1Parse g (const 0) w

type LR1LookAhead = Token -- Single Token of lookahead for LR1

lr1Goto :: Grammar () -> Goto LR1LookAhead
lr1Goto g = goto g (lr1Closure g)

type LR1State = LRState LR1LookAhead

lr1S0 :: Grammar () -> LRState LR1LookAhead
lr1S0 = lrS0 EOF

lr1Items :: Grammar () -> Set (LRState LR1LookAhead)
lr1Items g = items g (lr1Goto g) (lr1Closure g) (lr1S0 g)

lr1Parse :: Grammar () -> Action ast -> [Token] -> Maybe ast
lr1Parse g = lrParse g (lr1Table g) (lr1Goto g) (lr1Closure g) (lr1S0 g)

