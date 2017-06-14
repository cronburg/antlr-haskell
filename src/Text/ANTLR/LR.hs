{-# LANGUAGE ScopedTypeVariables, ExplicitForAll, DeriveGeneric, DeriveAnyClass
  , FlexibleContexts, StandaloneDeriving, OverloadedStrings, MonadComprehensions #-}
module Text.ANTLR.LR
  ( Item(..), ItemLHS(..)
  , kernel, items
  , slrClosure, slrGoto, slrItems, allSLRItems, slrTable, slrParse, slrRecognize
  , lr1Closure, lr1Goto, lr1Items, lr1Table, lr1Parse, lr1Recognize
  , LR1LookAhead
  , LRState, LRTable, LRAction(..)
  , lrParse, LRResult(..), glrParse, isAccept, isError
  ) where
import Text.ANTLR.Allstar.Grammar
import qualified Text.ANTLR.LL1 as LL
import Text.ANTLR.Parser
import Data.Maybe (catMaybes)
import Text.ANTLR.Set ( Set(..), fromList, empty, member, toList, size
  , union, (\\), insert, toList, singleton
  )
import qualified Text.ANTLR.Set as S
import qualified Text.ANTLR.MultiMap as M

--import Data.Map ( Map(..) )
--import qualified Data.Map as M

import Text.ANTLR.Pretty
import qualified Debug.Trace as D
import System.IO.Unsafe (unsafePerformIO)
uPIO = unsafePerformIO

trace = D.trace
--trace x y = y

data ItemLHS nts =
    Init   nts -- S' if S is the grammar start symbol
  | ItemNT nts -- wrapper around a NonTerminal
  deriving (Eq, Ord, Generic, Hashable)

instance (Prettify nts) => Prettify (ItemLHS nts) where
  prettify (Init nts)   = prettify nts >> pStr "_0"
  prettify (ItemNT nts) = prettify nts

instance (Show nts) => Show (ItemLHS nts) where
  show (Init nts)   = show nts ++ "'"
  show (ItemNT nts) = show nts

-- An Item is a production with a dot in it indicating how far
-- intso the production we have parsed:
--                         A        ->  α                          .     β
data Item a nts sts = Item (ItemLHS nts) (ProdElems nts sts) {- . -} (ProdElems nts sts) a
  deriving (Generic, Eq, Ord, Hashable, Show)

instance (Prettify a, Prettify nts, Prettify sts) => Prettify (Item a nts sts) where
  prettify (Item _A α β a) = do
    prettify _A
    pStr " -> "
    prettify α
    pStr " . "
    prettify β
    pParens (prettify a)

type Closure a nts sts = Set (Item a nts sts) -> Set (Item a nts sts)

type SLRClosure nts sts = Closure () nts sts
type LR1Closure nts sts = Closure (LR1LookAhead sts) nts sts

slrClosure ::
  forall nts sts.
  ( Eq sts
  , Ord nts, Ord sts
  , Hashable sts, Hashable nts)
  => Grammar () nts sts -> SLRClosure nts sts
slrClosure g is' = let

    closure' :: SLRClosure nts sts
    closure' _J = let
      add = fromList
            [ Item (ItemNT _B) [] γ ()
            | Item _A α rst@(pe@(NT _B) : β) () <- toList _J
            , not $ null rst
            , isNT pe
            , Production _ (Prod _ γ) <- prodsFor g _B
            ]
      in case size $ add \\ _J of
        0 -> _J `union` add
        _ -> closure' $ _J `union` add

  in closure' is'

lr1Closure ::
  forall nts sts.
  ( Eq nts, Eq sts
  , Ord nts, Ord sts, Ord sts
  , Hashable sts, Hashable sts, Hashable nts)
  => Grammar () nts sts -> LR1Closure nts sts
lr1Closure g is' = let

    tokenToProdElem (Icon a) = [T a]
    tokenToProdElem _ = []

    closure' :: LR1Closure nts sts
    closure' _J = let
      add = fromList
            -- TODO: Handle IconEOF in LL.first set calculation properly?:
            [ Item (ItemNT _B) [] γ (if b == IconEps then IconEOF else b)
            | Item _A α rst@(pe@(NT _B) : β) a <- toList _J
            , not $ null rst
            , isNT pe
            , Production _ (Prod _ γ) <- prodsFor g _B
            , b <- toList $ LL.first g (β ++ tokenToProdElem a)
            ]
      in case size $ add \\ _J of
        0 -> _J `union` add
        _ -> closure' $ _J `union` add

  in closure' is'

type Goto a nts sts = LRState a nts sts -> ProdElem nts sts -> LRState a nts sts

goto ::
  ( Ord a, Ord nts, Ord sts
  , Hashable sts, Hashable nts, Hashable a)
  => Grammar () nts sts -> Closure a nts sts -> Goto a nts sts
goto g closure is _X = closure $ fromList
  [ Item _A (_X : α) β  a
  | Item _A α (_X' : β) a <- toList is
  , _X == _X'
  ]

slrGoto ::
  forall nts sts.
  ( Eq nts, Eq sts
  , Ord nts, Ord sts
  , Hashable sts, Hashable nts)
  => Grammar () nts sts -> Goto () nts sts
slrGoto g = goto g (slrClosure g)

items ::
  forall a nts sts.
  ( Ord a, Ord nts, Ord sts
  , Eq nts, Eq sts
  , Hashable a, Hashable sts, Hashable nts)
  => Grammar () nts sts -> Goto a nts sts -> Closure a nts sts -> LRState a nts sts -> Set (LRState a nts sts)
items g goto closure s0 = let
    items' :: Set (LRState a nts sts) -> Set (LRState a nts sts)
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

kernel ::
  ( Ord a, Ord sts, Ord nts
  , Hashable a, Hashable sts, Hashable nts)
  => Set (Item a nts sts) -> Set (Item a nts sts)
kernel = let
    kernel' (Item (Init   _) _  _ _) = True
    kernel' (Item (ItemNT _) [] _ _) = False
    kernel' _ = True
  in S.filter kernel'

-- Generate the set of all possible Items for a given grammar:
allSLRItems ::
  forall nts sts.
  ( Eq nts, Eq sts
  , Ord nts, Ord sts
  , Hashable sts, Hashable nts)
  => Grammar () nts sts -> Set (SLRItem nts sts)
allSLRItems g = fromList
    [ Item (Init $ s0 g) [] [NT $ s0 g] ()
    , Item (Init $ s0 g) [NT $ s0 g] [] ()
    ]
  `union`
  fromList
    [ Item (ItemNT nts) (reverse $ take n γ) (drop n γ) ()
    | nts <- toList $ ns g
    , Production _ (Prod _ γ) <- prodsFor g nts
    , n <- [0..length γ]
    ]

type LRState a nts sts = Set (Item a nts sts)
-- TODO: String should be an arbitrary Eq and Ord "Symbol" type
type LRTable a nts sts = M.Map (LRState a nts sts, Icon sts) (LRAction a nts sts)

data LRAction a nts sts =
    Shift  (LRState a nts sts)
  | Reduce (Production () nts sts)
  | Accept
  | Error
  deriving (Generic, Eq, Ord, Hashable, Show)

instance
  ( Prettify a, Prettify nts, Prettify sts
  , Hashable a, Hashable sts, Hashable nts
  , Eq a, Eq sts, Eq nts)
  => Prettify (LRAction a nts sts) where
  prettify (Shift ss) = pStr "Shift  {" >> prettify ss >> pLine "}"
  prettify (Reduce p) = pStr "Reduce  " >> prettify p  >> pLine ""
  prettify Accept     = pStr "Accept"
  prettify Error      = pStr "Error"

type SLRItem  nts sts = Item    () nts sts
type SLRState nts sts = LRState () nts sts
type SLRTable nts sts = LRTable () nts sts

lrS0 ::
  ( Ord a, Ord sts, Ord nts
  , Hashable a, Hashable sts, Hashable nts)
  => a -> Grammar () nts sts -> LRState a nts sts
lrS0 a g = singleton $ Item (Init $ s0 g) [] [NT $ s0 g] a

slrS0 ::
  ( Ord sts, Ord nts
  , Hashable sts, Hashable nts)
  => Grammar () nts sts -> SLRState nts sts
slrS0 = lrS0 ()

slrItems ::
  forall nts sts.
  ( Eq nts, Eq sts
  , Ord nts, Ord sts
  , Hashable sts, Hashable nts)
  => Grammar () nts sts -> Set (Set (SLRItem nts sts))
slrItems g = items g (slrGoto g) (slrClosure g) (slrS0 g)

slrTable ::
  forall nts sts.
  ( Eq nts, Eq sts
  , Ord nts, Ord sts
  , Hashable nts, Hashable sts)
  => Grammar () nts sts -> SLRTable nts sts
slrTable g = let

    --slr' :: a -> b -> b
    --slr' :: Set Item -> Item -> LRTable -> LRTable
    --slr' :: SLRState nts sts -> SLRTable nts sts
    slr' _Ii = let
        --slr'' :: SLRItem nts sts -> SLRTable nts sts
        slr'' (Item (ItemNT nts) α (T a:β) ()) = --uPIO (prints ("TABLE:", a, slrGoto g _Ii $ T a, _Ii)) `seq`
                  [((_Ii, Icon a), Shift $ slrGoto g _Ii $ T a)]
        slr'' (Item (Init   nts) α (T a:β) ()) = [((_Ii, Icon a), Shift $ slrGoto g _Ii $ T a)]
        slr'' (Item (ItemNT nts) α [] ())      =
                                          [ ((_Ii, a), Reduce (Production nts (Prod Pass $ reverse α)))
                                          | a <- (toList . LL.follow g) nts
                                          ]
        slr'' (Item (Init nts) α [] ())   = [((_Ii, IconEOF), Accept)]
        slr'' _ = []
      in concat (S.toList $ S.map slr'' _Ii)

  in M.fromList $ concat $ S.map slr' $ slrItems g

type LR1Item  nts sts = Item    (LR1LookAhead sts) nts sts
type LR1Table nts sts = LRTable (LR1LookAhead sts) nts sts

lr1Table :: forall nts sts.
  ( Eq nts, Eq sts
  , Ord nts, Ord sts
  , Hashable sts, Hashable nts)
  => Grammar () nts sts -> LR1Table nts sts
lr1Table g = let
    --lr1' :: LR1State nts sts -> LR1Table nts sts
    lr1' _Ii = let
        --lr1'' :: LR1Item nts sts -> LR1Table nts sts
        lr1'' (Item (ItemNT nts) α (T a:β) _) = --uPIO (prints ("TABLE:", a, slrGoto g _Ii $ T a, _Ii)) `seq`
                  Just ((_Ii, Icon a), Shift $ lr1Goto g _Ii $ T a)
        lr1'' (Item (Init   nts) α (T a:β) _) = Just ((_Ii, Icon a), Shift $ lr1Goto g _Ii $ T a)
        lr1'' (Item (ItemNT nts) α [] a)      = Just ((_Ii,       a), Reduce (Production nts (Prod Pass $ reverse α)))
        lr1'' (Item (Init nts) α [] IconEOF)  = Just ((_Ii, IconEOF), Accept)
        lr1'' _ = Nothing
      in catMaybes (S.toList $ S.map lr1'' _Ii)

  in M.fromList $ concat (S.map lr1' $ lr1Items g)

type Config a nts ts t = ([LRState a nts ts], [t])

look ::
  ( Ord a, Ord nts, Ord sts
  , Eq sts
  , Hashable a, Hashable sts, Hashable nts)
  => (LRState a nts sts, Icon sts) -> LRTable a nts sts -> Set (LRAction a nts sts)
look (s,a) tbl = --uPIO (prints ("lookup:", s, a, M.lookup (s, a) act)) `seq`
    M.lookup (s, a) tbl

data LRResult a nts ts t ast =
    ErrorNoAction (Config a nts ts t) [ast]
  | ErrorAccept   (Config a nts ts t) [ast]
  | ResultSet     (Set (LRResult a nts ts t ast))
  | ResultAccept  ast
  deriving (Eq, Ord, Show, Generic, Hashable)

instance  ( Prettify t, Prettify ast, Prettify a, Prettify nts, Prettify ts
          , Hashable a, Hashable ts, Hashable nts, Eq a, Eq ts, Eq nts, Eq t, Eq ast
          , Hashable ast, Hashable t)
  => Prettify (LRResult a nts ts t ast) where
  
  prettify (ErrorNoAction (s:states, a:ws) asts) = do
    pStr "Error: Current input = "
    prettify a
    pLine ""
    incrIndent 7
    
    pStr "Current state = "
    prettify s
    pLine ""

    pStr "Rest of input = "
    prettify ws
    pLine ""
    
  prettify (ErrorAccept   (s:states, a:ws) asts) = do
    pStr "Error: Current input = "
    prettify a
    pLine ""
    incrIndent 7
    
    pStr "Current state = "
    prettify s
    pLine ""
    
    pStr "Rest of input = "
    prettify ws
    pLine ""
 
  prettify (ResultSet s) = prettify s

  prettify (ResultAccept ast)             = prettify ast

isAccept (ResultAccept _) = True
isAccept _                = False

isError (ResultAccept _) = False
isError _                = True

--getResults :: (Eq ast, Eq nts, Eq ts, Eq t, Eq a) => Set (LRResult a nts ts t ast) -> [ast]
getAccepts xs = [x | x <- xs, isAccept x]

lrParse ::
  forall ast a nts t.
  ( Ord a, Ord nts, Ord (Sym t), Ord t, Ord (StripEOF (Sym t))
  , Eq nts, Eq (Sym t), Eq (StripEOF (Sym t))
  , Ref t, HasEOF (Sym t)
  , Hashable (Sym t), Hashable t, Hashable a, Hashable nts, Hashable (StripEOF (Sym t))
  , Prettify a, Prettify t, Prettify nts, Prettify (StripEOF (Sym t)))
  => Grammar () nts (StripEOF (Sym t)) -> LRTable a nts (StripEOF (Sym t)) -> Goto a nts (StripEOF (Sym t))
  -> Closure a nts (StripEOF (Sym t))  -> LRState a nts (StripEOF (Sym t)) -> Action ast nts t
  -> [t] -> LRResult a nts (StripEOF (Sym t)) t ast
lrParse g tbl goto closure s_0 act w = let
  
    lr :: Config a nts (StripEOF (Sym t)) t -> [ast] -> LRResult a nts (StripEOF (Sym t)) t ast
    lr (s:states, a:ws) asts = let
        
        lr' :: LRAction a nts (StripEOF (Sym t)) -> LRResult a nts (StripEOF (Sym t)) t ast
        lr' Accept = case length asts of
              1 -> ResultAccept $ head asts
              _ -> ErrorAccept (s:states, a:ws) asts
        lr' Error     = ErrorNoAction (s:states, a:ws) asts
        lr' (Shift t) = trace ("Shift: " ++ pshow' t) $ lr (t:s:states, ws) $ act (TermE a) : asts
        lr' (Reduce p@(Production _A (Prod _ β))) = let
              ss'@(t:_) = drop (length β) (s:states)
            in trace ("Reduce: " ++ pshow' p)
               lr (goto t (NT _A) : ss', a:ws)
                  (act (NonTE (_A, β, reverse $ take (length β) asts)) : drop (length β) asts)

      -- TODO: handle empty file test case
        lookVal = case stripEOF $ getSymbol a of
                    Just sym -> look (s, Icon sym) tbl
                    Nothing  -> look (s, IconEOF)  tbl

      in if S.null lookVal
          then ErrorNoAction (s:states, a:ws) asts
          else lr' $ (head . S.toList) lookVal

  in lr ([closure s_0], w) []

slrParse ::
  ( Eq (Sym nts), Eq (Sym t), Eq (StripEOF (Sym t))
  , Ref t, HasEOF (Sym t)
  , Ord nts, Ord (Sym t), Ord t, Ord (StripEOF (Sym t))
  , Hashable nts, Hashable (Sym t), Hashable t, Hashable (StripEOF (Sym t))
  , Prettify t, Prettify nts, Prettify (StripEOF (Sym t)))
  => Grammar () nts (StripEOF (Sym t)) -> Action ast nts t -> [t]
  -> LRResult () nts (StripEOF (Sym t)) t ast
slrParse g = lrParse g (slrTable g) (slrGoto g) (slrClosure g) (slrS0 g)

slrRecognize ::
  ( Eq (Sym nts), Eq (Sym t), Eq (StripEOF (Sym t))
  , Ref t, HasEOF (Sym t)
  , Ord nts, Ord (Sym t), Ord t, Ord (StripEOF (Sym t))
  , Hashable nts, Hashable (Sym t), Hashable t, Hashable (StripEOF (Sym t))
  , Prettify t, Prettify nts, Prettify (StripEOF (Sym t)))
  => Grammar () nts (StripEOF (Sym t)) -> [t] -> Bool
slrRecognize g w = isAccept $ slrParse g (const 0) w

lr1Recognize ::
  ( Eq (Sym nts), Eq (Sym t), Eq (StripEOF (Sym t))
  , Ref t, HasEOF (Sym t)
  , Ord nts, Ord (Sym t), Ord t, Ord (StripEOF (Sym t))
  , Hashable nts, Hashable (Sym t), Hashable t, Hashable (StripEOF (Sym t))
  , Prettify t, Prettify nts, Prettify (StripEOF (Sym t)))
  => Grammar () nts (StripEOF (Sym t)) -> [t] -> Bool
lr1Recognize g w = isAccept $ lr1Parse g (const 0) w

type LR1LookAhead sts = Icon sts -- Single Icon of lookahead for LR1

lr1Goto ::
  ( Eq nts, Eq sts
  , Ord nts, Ord sts
  , Hashable sts, Hashable nts)
  => Grammar () nts sts -> Goto (LR1LookAhead sts) nts sts
lr1Goto g = goto g (lr1Closure g)

type LR1State nts t = LRState (LR1LookAhead t) nts t

lr1S0 ::
  ( Eq sts
  , Ord sts, Ord nts
  , Hashable sts, Hashable nts)
  => Grammar () nts sts -> LRState (LR1LookAhead sts) nts sts
lr1S0 = lrS0 IconEOF

lr1Items ::
  ( Eq sts, Eq sts
  , Ord nts, Ord sts
  , Hashable sts, Hashable nts)
  => Grammar () nts sts -> Set (LRState (LR1LookAhead sts) nts sts)
lr1Items g = items g (lr1Goto g) (lr1Closure g) (lr1S0 g)

lr1Parse ::
  ( Eq (Sym nts), Eq (Sym t), Eq (StripEOF (Sym t))
  , Ref t, HasEOF (Sym t)
  , Ord nts, Ord (Sym t), Ord t, Ord (StripEOF (Sym t))
  , Hashable nts, Hashable (Sym t), Hashable t, Hashable (StripEOF (Sym t))
  , Prettify t, Prettify nts, Prettify (StripEOF (Sym t)))
  => Grammar () nts (StripEOF (Sym t)) -> Action ast nts t -> [t]
  -> LRResult (LR1LookAhead (StripEOF (Sym t))) nts (StripEOF (Sym t)) t ast
lr1Parse g = lrParse g (lr1Table g) (lr1Goto g) (lr1Closure g) (lr1S0 g)

glrParse' ::
  forall ast a nts t.
  ( Ord a, Ord nts, Ord (Sym t), Ord t, Ord (StripEOF (Sym t)), Ord ast
  , Eq nts, Eq (Sym t), Eq (StripEOF (Sym t)), Eq ast
  , Ref t, HasEOF (Sym t)
  , Hashable (Sym t), Hashable t, Hashable a, Hashable nts, Hashable (StripEOF (Sym t)), Hashable ast
  , Prettify a, Prettify t, Prettify nts, Prettify (StripEOF (Sym t)))
  => Grammar () nts (StripEOF (Sym t)) -> LRTable a nts (StripEOF (Sym t)) -> Goto a nts (StripEOF (Sym t))
  -> Closure a nts (StripEOF (Sym t))  -> LRState a nts (StripEOF (Sym t)) -> Action ast nts t
  -> [t] -> LRResult a nts (StripEOF (Sym t)) t ast
glrParse' g tbl goto closure s_0 act w = let
  
    lr :: Config a nts (StripEOF (Sym t)) t -> [ast] -> LRResult a nts (StripEOF (Sym t)) t ast
    lr (s:states, a:ws) asts = let
        
        lr' :: LRAction a nts (StripEOF (Sym t)) -> LRResult a nts (StripEOF (Sym t)) t ast
        lr' Accept    = case length asts of
              1 -> ResultAccept $ head asts
              _ -> ErrorAccept (s:states, a:ws) asts
        lr' Error     = ErrorNoAction (s:states, a:ws) asts
        lr' (Shift t) = trace ("Shift: " ++ pshow' t) $ lr (t:s:states, ws) $ act (TermE a) : asts
        lr' (Reduce p@(Production _A (Prod _ β))) = let
              ss'@(t:_) = drop (length β) (s:states)
            in trace ("Reduce: " ++ pshow' p)
               lr (goto t (NT _A) : ss', a:ws)
                  (act (NonTE (_A, β, reverse $ take (length β) asts)) : drop (length β) asts)

        lookVal = case stripEOF $ getSymbol a of
                    Just sym -> look (s, Icon sym) tbl
                    Nothing  -> look (s, IconEOF)  tbl

        parseResults = S.map lr' lookVal
        justAccepts  = getAccepts parseResults

      in if S.null lookVal
          then ErrorNoAction (s:states, a:ws) asts
          else (if S.null justAccepts
                  then (case S.size parseResults of
                          0 -> undefined
                          1 -> S.findMin parseResults
                          _ -> ResultSet parseResults)
                  else ResultSet justAccepts)

  in lr ([closure s_0], w) []

glrParse g = glrParse' g (lr1Table g) (lr1Goto g) (lr1Closure g) (lr1S0 g)

