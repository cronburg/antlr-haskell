{-# LANGUAGE ScopedTypeVariables, ExplicitForAll #-}
module Text.ANTLR.LR1
  ( InputSymbol(..), Action, Item(..), ItemLHS(..), ParseEvent(..)
  , kernel, items
  , slrClosure, slrGoto, slrItems, allSLRItems, slrTable, slrParse, slrRecognize
  , lr1Closure, lr1Goto, lr1Items, lr1Table, lr1Parse, lr1Recognize
  , LRState, LRTable, LRAction(..)
  , lrParse
  ) where
import Text.ANTLR.Allstar.Grammar
import qualified Text.ANTLR.LL1 as LL
import Text.ANTLR.LL1 (InputSymbol(..))

import Data.Set ( Set(..), fromList, empty, member, toList, size
  , union, (\\), insert, toList, singleton
  )
import qualified Data.Set as S
import Data.Map ( Map(..) )
import qualified Data.Map as M

import System.IO.Unsafe (unsafePerformIO)
uPIO = unsafePerformIO

data ItemLHS nt =
    Init   nt -- S' if S is the grammar start symbol
  | ItemNT nt -- wrapper around a NonTerminal
  deriving (Eq, Ord, Show)

-- An Item is a production with a dot in it indicating how far
-- into the production we have parsed:
--                     A        ->  α                .     β
data Item a nt t = Item (ItemLHS nt) (Symbols nt t) {- . -} (Symbols nt t) a
  deriving (Eq, Ord, Show)

type Closure a nt t = Set (Item a nt t) -> Set (Item a nt t)

type SLRClosure nt t = Closure () nt t
type LR1Closure nt t = Closure (LR1LookAhead t) nt t

slrClosure ::
  forall nt t. (NonTerminal nt, Terminal t, Ord nt, Ord t)
  => Grammar () nt t -> SLRClosure nt t
slrClosure g is' = let

    closure' :: SLRClosure nt t
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

lr1Closure ::
  forall nt t. (NonTerminal nt, Terminal t, Ord nt, Ord t)
  => Grammar () nt t -> LR1Closure nt t
lr1Closure g is' = let

    tokenToProdElem (InputSymbol a) = [T a]
    tokenToProdElem _ = []

    closure' :: LR1Closure nt t
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

type Goto a nt t = LRState a nt t -> ProdElem nt t -> LRState a nt t

goto :: (Ord a, Ord nt, Ord t) => Grammar () nt t -> Closure a nt t -> Goto a nt t
goto g closure is _X = closure $ fromList
  [ Item _A (_X : α) β  a
  | Item _A α (_X' : β) a <- toList is
  , _X == _X'
  ]

slrGoto ::
  forall nt t. (NonTerminal nt, Terminal t, Ord nt, Ord t)
  => Grammar () nt t -> Goto () nt t
slrGoto g = goto g (slrClosure g)

items ::
  forall a nt t. (Ord a, NonTerminal nt, Terminal t, Ord nt, Ord t)
  => Grammar () nt t -> Goto a nt t -> Closure a nt t -> LRState a nt t -> Set (LRState a nt t)
items g goto closure s0 = let
    items' :: Set (LRState a nt t) -> Set (LRState a nt t)
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

kernel :: Set (Item a nt t) -> Set (Item a nt t)
kernel = let
    kernel' (Item (Init   _) _  _ _) = True
    kernel' (Item (ItemNT _) [] _ _) = False
    kernel' _ = True
  in S.filter kernel'

-- Generate the set of all possible Items for a given grammar:
allSLRItems ::
  forall nt t. (NonTerminal nt, Terminal t, Ord nt, Ord t)
  => Grammar () nt t -> Set (SLRItem nt t)
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

type LRState a nt t = Set (Item a nt t)
type LRTable a nt t = Map (LRState a nt t, InputSymbol t) (LRAction a nt t)

data LRAction a nt t =
    Shift  (LRState a nt t)
  | Reduce (Production () nt t)
  | Accept
  | Error
  deriving (Eq, Ord, Show)

type SLRItem  nt t = Item    () nt t
type SLRState nt t = LRState () nt t
type SLRTable nt t = LRTable () nt t

lrS0 :: a -> Grammar () nt t -> LRState a nt t
lrS0 a g = singleton $ Item (Init $ s0 g) [] [NT $ s0 g] a

slrS0 :: Grammar () nt t -> SLRState nt t
slrS0 = lrS0 ()

slrItems ::
  forall nt t. (NonTerminal nt, Terminal t, Ord nt, Ord t)
  => Grammar () nt t -> Set (Set (SLRItem nt t))
slrItems g = items g (slrGoto g) (slrClosure g) (slrS0 g)

slrTable ::
  forall nt t. (NonTerminal nt, Terminal t, Ord nt, Ord t)
  => Grammar () nt t -> SLRTable nt t
slrTable g = let

    --slr' :: a -> b -> b
    --slr' :: Set Item -> Item -> LRTable -> LRTable
    slr' :: SLRState nt t -> SLRTable nt t
    slr' _Ii = let
        slr'' :: SLRItem nt t -> SLRTable nt t
        slr'' (Item (ItemNT nt) α (T a:β) ()) = --uPIO (print ("TABLE:", a, slrGoto g _Ii $ T a, _Ii)) `seq`
                  M.singleton (_Ii, InputSymbol a) (Shift $ slrGoto g _Ii $ T a)
        slr'' (Item (Init   nt) α (T a:β) ()) = M.singleton (_Ii, InputSymbol a) (Shift $ slrGoto g _Ii $ T a)
        slr'' (Item (ItemNT nt) α [] ())      = M.fromList
                                          [ ((_Ii, a), Reduce (nt, Prod $ reverse α))
                                          | a <- (toList . LL.follow g) nt
                                          ]
        slr'' (Item (Init nt) α [] ())   = M.singleton (_Ii, LL.EOF) Accept
        slr'' _ = M.empty
      in S.fold M.union M.empty (S.map slr'' _Ii)

  in S.fold M.union M.empty $ S.map slr' $ slrItems g

type LR1Item  nt t = Item    (LR1LookAhead t) nt t
type LR1Table nt t = LRTable (LR1LookAhead t) nt t

lr1Table :: forall nt t. (NonTerminal nt, Terminal t, Ord nt, Ord t)
  => Grammar () nt t -> LR1Table nt t
lr1Table g = let
    lr1' :: LR1State nt t -> LR1Table nt t
    lr1' _Ii = let
        lr1'' :: LR1Item nt t -> LR1Table nt t
        lr1'' (Item (ItemNT nt) α (T a:β) _) = --uPIO (print ("TABLE:", a, slrGoto g _Ii $ T a, _Ii)) `seq`
                  M.singleton (_Ii, InputSymbol a) (Shift $ lr1Goto g _Ii $ T a)
        lr1'' (Item (Init   nt) α (T a:β) _) = M.singleton (_Ii, InputSymbol a) (Shift $ lr1Goto g _Ii $ T a)
        lr1'' (Item (ItemNT nt) α [] a)      = M.singleton (_Ii,       a) (Reduce (nt, Prod $ reverse α))
        lr1'' (Item (Init nt) α [] EOF)   = M.singleton (_Ii, LL.EOF) Accept
        lr1'' _ = M.empty
      in S.fold M.union M.empty (S.map lr1'' _Ii)

  in S.fold M.union M.empty $ S.map lr1' $ lr1Items g

type Config a nt t = ([LRState a nt t], [InputSymbol t])

look :: (Ord a, Ord nt, Ord t)
  => (LRState a nt t, InputSymbol t) -> LRTable a nt t -> Maybe (LRAction a nt t)
look (s,a) tbl = --uPIO (print ("lookup:", s, a, M.lookup (s, a) act)) `seq`
    M.lookup (s, a) tbl

-- TODO: unify with LL
data ParseEvent ast nt t =
    InputSymbolE (InputSymbol t)
  | NonTE  (nt, Symbols nt t, [ast])

type Action ast nt t = ParseEvent ast nt t -> ast

lrParse ::
  forall ast a nt t. (Ord a, NonTerminal nt, Terminal t, Ord nt, Ord t)
  => Grammar () nt t -> LRTable a nt t -> Goto a nt t
  -> Closure a nt t -> LRState a nt t -> Action ast nt t
  -> [InputSymbol t] -> Maybe ast
lrParse g tbl goto closure s_0 act w = let
  
    lr :: Config a nt t -> [ast] -> Maybe ast
    lr (s:states, a:ws) asts = let
        
        lr' :: Maybe (LRAction a nt t) -> Maybe ast
        lr' Nothing          = Nothing
        lr' (Just Accept)    = case length asts of
              1 -> Just $ head asts
              _ -> Nothing
        lr' (Just Error)     = Nothing
        lr' (Just (Shift t)) = lr (t:s:states, ws) $ act (InputSymbolE a) : asts
        lr' (Just (Reduce (_A, Prod β))) = let
              ss'@(t:_) = drop (length β) (s:states)
            in lr (goto t (NT _A) : ss', a:ws)
                  ((act $ NonTE (_A, β, reverse $ take (length β) asts)) : drop (length β) asts)

      in lr' $ look (s,a) tbl

  in lr ([closure s_0], w) []

slrParse :: (NonTerminal nt, Terminal t, Ord nt, Ord t)
  => Grammar () nt t -> Action ast nt t -> [InputSymbol t] -> Maybe ast
slrParse g = lrParse g (slrTable g) (slrGoto g) (slrClosure g) (slrS0 g)

slrRecognize :: (NonTerminal nt, Terminal t, Ord nt, Ord t)
  => Grammar () nt t -> [InputSymbol t] -> Bool
slrRecognize g w = (Nothing /=) $ slrParse g (const 0) w

lr1Recognize :: (NonTerminal nt, Terminal t, Ord nt, Ord t)
  => Grammar () nt t -> [InputSymbol t] -> Bool
lr1Recognize g w = (Nothing /=) $ lr1Parse g (const 0) w

type LR1LookAhead t = InputSymbol t -- Single InputSymbol of lookahead for LR1

lr1Goto :: (NonTerminal nt, Terminal t, Ord nt, Ord t)
  => Grammar () nt t -> Goto (LR1LookAhead t) nt t
lr1Goto g = goto g (lr1Closure g)

type LR1State nt t = LRState (LR1LookAhead t) nt t

lr1S0 :: Grammar () nt t -> LRState (LR1LookAhead t) nt t
lr1S0 = lrS0 EOF

lr1Items :: (NonTerminal nt, Terminal t, Ord nt, Ord t)
  => Grammar () nt t -> Set (LRState (LR1LookAhead t) nt t)
lr1Items g = items g (lr1Goto g) (lr1Closure g) (lr1S0 g)

lr1Parse :: (NonTerminal nt, Terminal t, Ord nt, Ord t)
  => Grammar () nt t -> Action ast nt t -> [InputSymbol t] -> Maybe ast
lr1Parse g = lrParse g (lr1Table g) (lr1Goto g) (lr1Closure g) (lr1S0 g)

