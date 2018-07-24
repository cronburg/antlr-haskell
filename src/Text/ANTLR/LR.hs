{-# LANGUAGE ScopedTypeVariables, ExplicitForAll, DeriveGeneric, DeriveAnyClass
  , FlexibleContexts, StandaloneDeriving, OverloadedStrings, MonadComprehensions
  , InstanceSigs, DeriveDataTypeable, DeriveLift #-}
module Text.ANTLR.LR
  ( Item(..), ItemLHS(..)
  , kernel, items
  , slrClosure, slrGoto, slrItems, allSLRItems, slrTable, slrParse, slrRecognize
  , lr1Closure, lr1Goto, lr1Items, lr1Table, lr1Parse, lr1Recognize
  , LR1LookAhead
  , CoreLRState, CoreLR1State, CoreSLRState, LRTable, LRAction(..)
  , lrParse, LRResult(..), LR1Result(..), glrParse, glrParseInc, isAccept, isError
  , lr1S0, glrParseInc'
  ) where
import Text.ANTLR.Grammar
import qualified Text.ANTLR.LL1 as LL
import Text.ANTLR.Parser
import Data.Maybe (catMaybes, mapMaybe, fromMaybe)
import Text.ANTLR.Set ( Set(..), fromList, empty, member, toList, size
  , union, (\\), insert, toList, singleton
  )
import qualified Text.ANTLR.Set as S
import qualified Text.ANTLR.MultiMap as M

--import Data.Map ( Map(..) )
import qualified Data.Map as M1
import Data.Data (Data(..))
import Language.Haskell.TH.Lift (Lift(..))

import Text.ANTLR.Pretty
import qualified Debug.Trace as D
import System.IO.Unsafe (unsafePerformIO)
uPIO = unsafePerformIO

--trace = D.trace
trace x y = y

data ItemLHS nts =
    Init   nts -- S' if S is the grammar start symbol
  | ItemNT nts -- wrapper around a NonTerminal
  deriving (Eq, Ord, Generic, Hashable, Data, Lift)

-- An Item is a production with a dot in it indicating how far
-- into the production we have parsed:
--                         A        ->  α                          .     β
data Item a nts sts = Item (ItemLHS nts) (ProdElems nts sts) {- . -} (ProdElems nts sts) a
  deriving (Generic, Eq, Ord, Hashable, Show, Data, Lift)

type Closure lrstate          = lrstate -> lrstate
type Goto nts sts lrstate     = M1.Map (lrstate, ProdElem nts sts) lrstate
type Goto' nts sts lrstate    = lrstate -> ProdElem nts sts -> lrstate
type LRTable nts sts lrstate  = M.Map (lrstate, Icon sts) (LRAction nts sts lrstate)

-- | CoreLRState is the one computed from the grammar (no information loss)
type CoreLRState a nts sts = Set (Item a nts sts)

type LR1Action nts sts lrstate  = LRAction nts sts lrstate
--type LR1Goto nts sts lrstate    = Goto nts sts lrstate
type LR1Closure lrstate         = Closure lrstate
type LR1Result lrstate t ast    = LRResult lrstate t ast
type LR1Item  nts sts           = Item    (LR1LookAhead sts) nts sts
type LR1Table nts sts lrstate   = LRTable nts sts lrstate
type LR1LookAhead sts           = Icon sts -- Single Icon of lookahead for LR1
type CoreLR1State nts sts       = Set (Item (LR1LookAhead sts) nts sts)

type SLRClosure lrstate = Closure lrstate
type SLRItem  nts sts = Item    () nts sts
type SLRTable nts sts lrstate = LRTable nts sts lrstate
type CoreSLRState nts sts = Set (Item () nts sts)

data LRAction nts sts lrstate =
    Shift  lrstate
  | Reduce (Production () nts sts)
  | Accept
  | Error
  deriving (Generic, Eq, Ord, Hashable, Show, Data, Lift)

type Config lrstate t = ([lrstate], [t])

data LRResult lrstate t ast =
    ErrorNoAction (Config lrstate t) [ast]
  | ErrorAccept   (Config lrstate t) [ast]
  | ResultSet     (Set (LRResult lrstate t ast))
  | ResultAccept  ast
  | ErrorTable    (Config lrstate t) [ast]
  deriving (Eq, Ord, Show, Generic, Hashable)

type Tokenizer t c = Set (StripEOF (Sym t)) -> [c] -> (t, [c])

instance (Prettify nts) => Prettify (ItemLHS nts) where
  prettify (Init nts)   = prettify nts >> pStr "_0"
  prettify (ItemNT nts) = prettify nts

instance (Show nts) => Show (ItemLHS nts) where
  show (Init nts)   = show nts ++ "'"
  show (ItemNT nts) = show nts

instance (Prettify a, Prettify nts, Prettify sts) => Prettify (Item a nts sts) where
  prettify (Item _A α β a) = do
    prettify _A
    pStr " -> "
    prettify α
    pStr " . "
    prettify β
    pParens (prettify a)

instance
  ( Prettify lrstate, Prettify nts, Prettify sts
  , Hashable lrstate, Hashable sts, Hashable nts
  , Eq lrstate, Eq sts, Eq nts)
  => Prettify (LRAction nts sts lrstate) where
  prettify (Shift ss) = pStr "Shift  {" >> prettify ss >> pLine "}"
  prettify (Reduce p) = pStr "Reduce  " >> prettify p  >> pLine ""
  prettify Accept     = pStr "Accept"
  prettify Error      = pStr "Error"

instance  ( Prettify t, Prettify ast, Prettify lrstate
          , Eq t, Eq ast, Eq lrstate
          , Hashable ast, Hashable t, Hashable lrstate)
  => Prettify (LRResult lrstate t ast) where
  
  prettify (ErrorNoAction (s:states, ws) asts) = do
    pStr "ErrorNoAction: Current input = '"
    if null ws then return () else prettify (head ws)
    pLine "'"
    incrIndent 7
    
    pStr "Current state = <"
    prettify s
    pLine ">"

    pStr "Rest of input = '"
    prettify ws
    pLine "'"
  
  prettify (ErrorTable (s:states, ws) asts) = do
    pStr "ErrorTable: Current input = '"
    if null ws then return () else prettify (head ws)
    pLine "'"
    incrIndent 7
    
    pStr "Current state = <"
    prettify s
    pLine ">"

    pStr "Rest of input = '"
    prettify ws
    pLine "'"
    
  prettify (ErrorAccept   (s:states, ws) asts) = do
    pStr "ErrorAccept: Current input = "
    (if null ws then return () else prettify (head ws))
    pLine ""
    incrIndent 7
    
    pStr "Current state = "
    prettify s
    pLine ""
    
    pStr "Rest of input = "
    prettify ws
    pLine ""
 
  prettify (ResultSet s) = pStr "ResultSet: " >> prettify s

  prettify (ResultAccept ast)             = pStr "ResultAccept: " >> prettify ast

slrClosure ::
  forall nts sts.
  ( Eq sts
  , Ord nts, Ord sts
  , Hashable sts, Hashable nts)
  => Grammar () nts sts -> SLRClosure (CoreSLRState nts sts)
slrClosure g is' = let

    closure' :: SLRClosure (CoreSLRState nts sts)
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
  => Grammar () nts sts -> Closure (CoreLR1State nts sts)
lr1Closure g is' = let

    tokenToProdElem (Icon a) = [T a]
    tokenToProdElem _ = []

    closure' :: Closure (CoreLR1State nts sts)
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

-- | Convert a function-based goto to a map-based one once we know the set of
-- all lrstates (sets of items for LR1) and all the production elements
convGoto :: (Hashable lrstate, Ord lrstate, Ord sts, Ord nts)
  => Grammar () nts sts -> Goto' nts sts lrstate -> Set lrstate -> Goto nts sts lrstate
convGoto g goto states = M1.fromList
  [ ((st0, e), goto st0 e)
  | st0 <- toList states
  , e   <- allProdElems g
  ]

allProdElems :: Grammar () nts ts -> [ProdElem nts ts]
allProdElems g =
      map NT (S.toList $ ns g)
  ++  map T  (S.toList $ ts g)

allProdElems' :: forall nts ts. (Bounded nts, Bounded ts, Enum nts, Enum ts)
  => [ProdElem nts ts]
allProdElems' =
      map NT ([minBound .. maxBound] :: [nts])
  ++  map T  ([minBound .. maxBound] :: [ts])

goto ::
  ( Ord a, Ord nts, Ord sts
  , Hashable sts, Hashable nts, Hashable a)
  => Grammar () nts sts -> Closure (CoreLRState a nts sts) -> Goto' nts sts (CoreLRState a nts sts)
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
  => Grammar () nts sts -> Goto' nts sts (CoreSLRState nts sts)
slrGoto g = goto g (slrClosure g)

items ::
  forall a nts sts.
  ( Ord a, Ord nts, Ord sts
  , Eq nts, Eq sts
  , Hashable a, Hashable sts, Hashable nts)
  => Grammar () nts sts -> Goto' nts sts (CoreLRState a nts sts) -> CoreLRState a nts sts -> Set (CoreLRState a nts sts)
items g goto s0 = let
    items' :: Set (CoreLRState a nts sts) -> Set (CoreLRState a nts sts)
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
  in items' $ singleton s0
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

lrS0 ::
  ( Ord a, Ord sts, Ord nts
  , Hashable a, Hashable sts, Hashable nts)
  => a -> Grammar () nts sts -> CoreLRState a nts sts
lrS0 a g = singleton $ Item (Init $ s0 g) [] [NT $ s0 g] a

slrS0 ::
  ( Ord sts, Ord nts
  , Hashable sts, Hashable nts)
  => Grammar () nts sts -> CoreLRState () nts sts
slrS0 = lrS0 ()

slrItems ::
  forall nts sts.
  ( Eq nts, Eq sts
  , Ord nts, Ord sts
  , Hashable sts, Hashable nts)
  => Grammar () nts sts -> Set (Set (SLRItem nts sts))
slrItems g = items g (slrGoto g) (slrClosure g $ slrS0 g)

slrTable ::
  forall nts sts.
  ( Eq nts, Eq sts
  , Ord nts, Ord sts
  , Hashable nts, Hashable sts)
  => Grammar () nts sts -> SLRTable nts sts (CoreSLRState nts sts)
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

lr1Table :: forall nts sts.
  ( Eq nts, Eq sts
  , Ord nts, Ord sts
  , Hashable sts, Hashable nts)
  => Grammar () nts sts -> LR1Table nts sts (CoreLR1State nts sts)
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

look ::
  ( Ord lrstate, Ord nts, Ord sts
  , Eq sts
  , Hashable lrstate, Hashable sts, Hashable nts)
  => (lrstate, Icon sts) -> LRTable nts sts lrstate -> Set (LRAction nts sts lrstate)
look (s,a) tbl = --uPIO (prints ("lookup:", s, a, M.lookup (s, a) act)) `seq`
    M.lookup (s, a) tbl

isAccept (ResultAccept _) = True
isAccept _                = False

isError (ResultAccept _) = False
isError _                = True

--getResults :: (Eq ast, Eq nts, Eq ts, Eq t, Eq a) => Set (LRResult a nts ts t ast) -> [ast]
getAccepts xs = fromList [x | x <- toList xs, isAccept x]

lrParse ::
  forall ast a nts t lrstate.
  ( Ord lrstate, Ord nts, Ord (Sym t), Ord t, Ord (StripEOF (Sym t))
  , Eq nts, Eq (Sym t), Eq (StripEOF (Sym t))
  , Ref t, HasEOF (Sym t)
  , Hashable (Sym t), Hashable t, Hashable lrstate, Hashable nts, Hashable (StripEOF (Sym t))
  , Prettify lrstate, Prettify t, Prettify nts, Prettify (StripEOF (Sym t)))
  => Grammar () nts (StripEOF (Sym t)) -> LRTable nts (StripEOF (Sym t)) lrstate -> Goto nts (StripEOF (Sym t)) lrstate
  -> lrstate -> Action ast nts t
  -> [t] -> LRResult lrstate t ast
lrParse g tbl goto s_0 act w = let
  
    lr :: Config lrstate t -> [ast] -> LRResult lrstate t ast
    lr (s:states, a:ws) asts = let
        
        lr' :: LRAction nts (StripEOF (Sym t)) lrstate -> LRResult lrstate t ast
        lr' Accept = case length asts of
              1 -> ResultAccept $ head asts
              _ -> ErrorAccept (s:states, a:ws) asts
        lr' Error     = ErrorNoAction (s:states, a:ws) asts
        lr' (Shift t) = trace ("Shift: " ++ pshow' t) $ lr (t:s:states, ws) $ act (TermE a) : asts
        lr' (Reduce p@(Production _A (Prod _ β))) = let
              ss'@(t:_) = drop (length β) (s:states)
              result =
                case (t, NT _A) `M1.lookup` goto of
                  Nothing -> ErrorTable (s:states, a:ws) asts
                  Just s  -> lr (s : ss', a:ws) (act (NonTE (_A, β, reverse $ take (length β) asts)) : drop (length β) asts)
            in trace ("Reduce: " ++ pshow' p) result

      -- TODO: handle empty file test case
        lookVal = case stripEOF $ getSymbol a of
                    Just sym -> look (s, Icon sym) tbl
                    Nothing  -> look (s, IconEOF)  tbl

      in if S.null lookVal
          then ErrorNoAction (s:states, a:ws) asts
          else lr' $ (head . S.toList) lookVal

  in lr ([s_0], w) []

slrParse ::
  ( Eq (Sym nts), Eq (Sym t), Eq (StripEOF (Sym t))
  , Ref t, HasEOF (Sym t)
  , Ord nts, Ord (Sym t), Ord t, Ord (StripEOF (Sym t))
  , Hashable nts, Hashable (Sym t), Hashable t, Hashable (StripEOF (Sym t))
  , Prettify t, Prettify nts, Prettify (StripEOF (Sym t)))
  => Grammar () nts (StripEOF (Sym t)) -> Action ast nts t -> [t]
  -> LRResult (CoreSLRState nts (StripEOF (Sym t))) t ast
slrParse g = lrParse g (slrTable g) (convGoto g (slrGoto g) (slrItems g)) (slrClosure g $ slrS0 g)

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

getLookAheads :: (Hashable sts, Hashable nts, Eq sts, Eq nts) => Set (LR1Item nts sts) -> Set sts
getLookAheads = let
    gLA (Item _ _ _ IconEOF)    = Nothing
    gLA (Item _ _ _ (Icon sts)) = Just sts
  in S.fromList . catMaybes . S.toList . S.map gLA

lr1Goto ::
  ( Eq nts, Eq sts
  , Ord nts, Ord sts
  , Hashable sts, Hashable nts)
  => Grammar () nts sts -> Goto' nts sts (CoreLR1State nts sts)
lr1Goto g = goto g (lr1Closure g)

lr1S0 ::
  ( Eq sts
  , Ord sts, Ord nts
  , Hashable sts, Hashable nts)
  => Grammar () nts sts -> CoreLRState (LR1LookAhead sts) nts sts
lr1S0 = lrS0 IconEOF

lr1Items ::
  ( Eq sts, Eq sts
  , Ord nts, Ord sts
  , Hashable sts, Hashable nts)
  => Grammar () nts sts -> Set (CoreLRState (LR1LookAhead sts) nts sts)
lr1Items g = items g (lr1Goto g) (lr1Closure g $ lr1S0 g)

lr1Parse ::
  ( Eq (Sym nts), Eq (Sym t), Eq (StripEOF (Sym t))
  , Ref t, HasEOF (Sym t)
  , Ord nts, Ord (Sym t), Ord t, Ord (StripEOF (Sym t))
  , Hashable nts, Hashable (Sym t), Hashable t, Hashable (StripEOF (Sym t))
  , Prettify t, Prettify nts, Prettify (StripEOF (Sym t)))
  => Grammar () nts (StripEOF (Sym t)) -> Action ast nts t -> [t]
  -> LRResult (CoreLR1State nts (StripEOF (Sym t))) t ast
lr1Parse g = lrParse g (lr1Table g) (convGoto g (lr1Goto g) (lr1Items g)) (lr1Closure g $ lr1S0 g)

glrParse' ::
  forall ast nts t lrstate.
  ( Ord lrstate, Ord nts, Ord (Sym t), Ord t, Ord (StripEOF (Sym t)), Ord ast
  , Eq nts, Eq (Sym t), Eq (StripEOF (Sym t)), Eq ast
  , Ref t, HasEOF (Sym t)
  , Hashable (Sym t), Hashable t, Hashable lrstate, Hashable nts, Hashable (StripEOF (Sym t)), Hashable ast
  , Prettify lrstate, Prettify t, Prettify nts, Prettify (StripEOF (Sym t)))
  => Grammar () nts (StripEOF (Sym t)) -> LRTable nts (StripEOF (Sym t)) lrstate -> Goto nts (StripEOF (Sym t)) lrstate
  -> lrstate -> Action ast nts t
  -> [t] -> LRResult lrstate t ast
glrParse' g tbl goto s_0 act w = let
  
    lr :: Config lrstate t -> [ast] -> LRResult lrstate t ast
    lr (s:states, a:ws) asts = let
        
        lr' :: LRAction nts (StripEOF (Sym t)) lrstate -> LRResult lrstate t ast
        lr' Accept    = case length asts of
              1 -> ResultAccept $ head asts
              _ -> ErrorAccept (s:states, a:ws) asts
        lr' Error     = ErrorNoAction (s:states, a:ws) asts
        lr' (Shift t) = trace ("Shift: " ++ pshow' t) $ lr (t:s:states, ws) $ act (TermE a) : asts
        lr' (Reduce p@(Production _A (Prod _ β))) = let
              ss'@(t:_) = drop (length β) (s:states)
              result =
                case (t, NT _A) `M1.lookup` goto of
                  Nothing -> ErrorTable (s:states, a:ws) asts
                  Just s  -> lr (s : ss', a:ws) (act (NonTE (_A, β, reverse $ take (length β) asts)) : drop (length β) asts)
            in trace ("Reduce: " ++ pshow' p) result

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

  in lr ([s_0], w) []

glrParse g = glrParse' g (lr1Table g) (convGoto g (lr1Goto g) (lr1Items g)) (lr1Closure g $ lr1S0 g)

glrParseInc' ::
  forall ast nts t c lrstate.
  ( Ord nts, Ord (Sym t), Ord t, Ord (StripEOF (Sym t)), Ord ast, Ord lrstate
  , Eq nts, Eq (Sym t), Eq (StripEOF (Sym t)), Eq ast
  , Ref t, HasEOF (Sym t)
  , Hashable (Sym t), Hashable t, Hashable nts, Hashable (StripEOF (Sym t)), Hashable ast, Hashable lrstate
  , Prettify t, Prettify nts, Prettify (StripEOF (Sym t)), Prettify lrstate
  , Eq c, Ord c, Hashable c)
  => Grammar () nts (StripEOF (Sym t)) -> LR1Table nts (StripEOF (Sym t)) lrstate -> Goto nts (StripEOF (Sym t)) lrstate
  -> lrstate -> M1.Map lrstate (Set (StripEOF (Sym t))) -> Action ast nts t
  -> Tokenizer t c -> [c] -> LR1Result lrstate c ast
glrParseInc' g tbl goto s_0 tokenizerFirstSets act tokenizer w = let
    
    lr :: Config lrstate c -> [ast] -> LR1Result lrstate c ast
    lr (s:states, cs) asts = let

        -- The set of token symbols that are feasible to be seen next given the
        -- current grammar context - i.e. the Set of LR1LookAheads stripped from
        -- the current state on top of the configuration stack. Luckily enough,
        -- it just so happens that the type stuffed inside an LR1 lookahead Icon
        -- is precisely the terminal symbol type that the tokenizer uses to name
        -- DFAs.
        dfaNames = fromMaybe (error "Impossible") $ s `M1.lookup` tokenizerFirstSets
        (a, ws) = tokenizer dfaNames cs
        
        lr' :: LR1Action nts (StripEOF (Sym t)) lrstate -> LR1Result lrstate c ast
        lr' Accept    = case length asts of
              1 -> ResultAccept $ head asts
              _ -> ErrorAccept (s:states, cs) asts
        lr' Error     = ErrorNoAction (s:states, cs) asts
        lr' (Shift t) = trace ("Shift: " ++ pshow' t) $ lr (t:s:states, ws) $ act (TermE a) : asts
        lr' (Reduce p@(Production _A (Prod _ β))) = let
              ss'@(t:_) = drop (length β) (s:states)
              result =
                case (t, NT _A) `M1.lookup` goto of
                  Nothing -> ErrorTable (s:states, cs) asts
                  Just s  -> lr (s : ss', cs) (act (NonTE (_A, β, reverse $ take (length β) asts)) : drop (length β) asts)
            in trace ("Reduce: " ++ pshow' p) result

        lookVal = case stripEOF $ getSymbol a of
                    Just sym -> look (s, Icon sym) tbl
                    Nothing  -> look (s, IconEOF)  tbl

        concatSets (ResultSet ss) ss' = ss' `S.union` ss
        concatSets r              ss' = r   `S.insert` ss'

        parseResults = S.foldr concatSets S.empty $ S.map lr' lookVal
        justAccepts  = getAccepts parseResults

      in if S.null lookVal
          then ErrorNoAction (s:states, cs) asts
          else (if S.null justAccepts
                  then (case S.size parseResults of
                          0 -> undefined
                          1 -> S.findMin parseResults
                          _ -> ResultSet parseResults)
                  else (case S.size justAccepts of
                          1 -> S.findMin justAccepts
                          _ -> ResultSet justAccepts))

  in lr ([s_0], w) []

glrParseInc g = let
   
    tbl = lr1Table g

    first s = let
        removeIcons (Icon t) = Just t
        removeIcons IconEps  = Nothing
        removeIcons IconEOF  = Nothing

        itemHeads (Item (Init   nt) _ [] _) = []
        itemHeads (Item (ItemNT nt) _ [] _) = S.toList $ LL.follow g nt -- TODO: Use stack context
        itemHeads (Item _ _ (b:bs)  _)      = S.toList $ LL.first  g [b]

      in S.fromList $ mapMaybe removeIcons $ concatMap itemHeads s

    --tokenizerFirstSets :: M1.Map lrstate (Set (StripEOF (Sym t)))
    tokenizerFirstSets = M1.fromList [ (s, first $ S.toList s) | ((s, _), _) <- M.toList tbl ]

  in glrParseInc' g (lr1Table g) (convGoto g (lr1Goto g) (lr1Items g)) (lr1Closure g $ lr1S0 g) tokenizerFirstSets

