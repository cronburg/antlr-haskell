{-# LANGUAGE ScopedTypeVariables, ExplicitForAll, DeriveGeneric, DeriveAnyClass
  , FlexibleContexts, StandaloneDeriving, OverloadedStrings, MonadComprehensions
  , InstanceSigs, DeriveDataTypeable, DeriveLift, ConstraintKinds #-}
{-|
  Module      : Text.ANTLR.LR
  Description : Entrypoint for all parsing algorithms based on LR
  Copyright   : (c) Karl Cronburg, 2018
  License     : BSD3
  Maintainer  : karl@cs.tufts.edu
  Stability   : experimental
  Portability : POSIX

-}
module Text.ANTLR.LR
  ( Item(..), ItemLHS(..)
  , kernel, items
  , slrClosure, slrGoto, slrItems, allSLRItems, slrTable, slrParse, slrRecognize
  , lr1Closure, lr1Goto, lr1Items, lr1Table, lr1Parse, lr1Recognize
  , LR1LookAhead
  , CoreLRState, CoreLR1State, CoreSLRState, LRTable, LRTable', LRAction(..)
  , lrParse, GLRResult(..), LRResult(..), LR1Result(..), glrParse, glrParseInc, isAccept, isError
  , lr1S0, glrParseInc', glrParseInc2
  , convGoto, convStateInt, convGotoStatesInt, convTableInt, tokenizerFirstSets
  , disambiguate
  , SLRClosure, SLRItem, SLRTable, Closure, LR1Item, Goto, Goto', Config, Tokenizer
  ) where
import Text.ANTLR.Grammar
import qualified Text.ANTLR.LL1 as LL
import Text.ANTLR.Parser
import Data.Maybe (catMaybes, mapMaybe, fromMaybe, fromJust)
import Text.ANTLR.Set ( Set(..), fromList, empty, member, toList, size
  , union, (\\), insert, toList, singleton
  )
import qualified Text.ANTLR.Set as S
import Text.ANTLR.Set (Hashable, Generic)
import qualified Text.ANTLR.MultiMap as M
import Text.ANTLR.Common

--import Data.Map ( Map(..) )
import qualified Data.Map as M1
import Data.Data (Data(..))
import Language.Haskell.TH.Lift (Lift(..))
import Data.List (sort)

import Text.ANTLR.Pretty
import qualified Debug.Trace as D
--import System.IO.Unsafe (unsafePerformIO)
--uPIO = unsafePerformIO

--trace = D.trace
trace x y = y

-- | The nonterminal symbol for which an item refers to.
data ItemLHS nts =
    Init   nts -- ^ This is S' if S is the grammar start symbol
  | ItemNT nts -- ^ Just an item wrapper around a nonterminal symbol
  deriving (Eq, Ord, Generic, Hashable, Data, Lift)

-- | An Item is a production with a dot in it indicating how far
--   into the production we have parsed:
--
-- @A ->  α . β@
--
data Item a nts sts = Item (ItemLHS nts) (ProdElems nts sts) {- . -} (ProdElems nts sts) a
  deriving (Generic, Eq, Ord, Hashable, Show, Data, Lift)

-- | Functions for computing the state (set of items) we can go to
--   next without consuming any input.
type Closure lrstate          = lrstate -> lrstate
-- | An LR goto implemented as one-to-one mapping.
type Goto nts sts lrstate     = M1.Map (lrstate, ProdElem nts sts) lrstate
-- | Function form of a 'Goto'
type Goto' nts sts lrstate    = lrstate -> ProdElem nts sts -> lrstate

-- | Ambiguous LR tables (can perform more than one action per @lrstate@)
type LRTable nts sts lrstate   = M.Map (lrstate, Icon sts) (LRAction nts sts lrstate)
-- | Disambiguated LR table (only one action performable per @lrstate@)
type LRTable' nts sts lrstate  = M1.Map (lrstate, Icon sts) (LRAction nts sts lrstate)

-- | CoreLRState is the one computed from the grammar (no information loss)
type CoreLRState a nts sts = Set (Item a nts sts)

-- | An LR1 action is just a regular 'LRAction'.
type LR1Action nts sts lrstate  = LRAction nts sts lrstate
-- | An LR1 closure is just a regular LR 'Closure'.
type LR1Closure lrstate         = Closure lrstate
-- | LR1 results are just 'LRResult's with identical tokens and characters
type LR1Result lrstate t ast    = LRResult lrstate t t ast
-- | GLR results are just 'LRResult's
type GLRResult lrstate c t ast  = LRResult lrstate c t ast
-- | An LR1 item is an 'Item' with one lookahead symbol.
type LR1Item  nts sts           = Item    (LR1LookAhead sts) nts sts
-- | An LR1 table is just an 'LRTable' in disguise.
type LR1Table nts sts lrstate   = LRTable nts sts lrstate
-- | LR1 lookahead is a single 'Icon'
type LR1LookAhead sts           = Icon sts
-- | An LR1 state is a set of items with one lookahead symbol.
type CoreLR1State nts sts       = Set (LR1Item nts sts)

-- | An SLRClosure is just a LR 'Closure' in disguise.
type SLRClosure lrstate = Closure lrstate
-- | SLR items have no lookahead.
type SLRItem  nts sts = Item    () nts sts
-- | An 'SLRTable' is just an 'LRTable' in disguise.
type SLRTable nts sts lrstate = LRTable nts sts lrstate
-- | An SLR state is a set of items without a lookahead.
type CoreSLRState nts sts = Set (Item () nts sts)

-- | The actions that an LR parser can tell the user about.
data LRAction nts sts lrstate =
    Shift  lrstate                          -- ^ Shift @lrstate@ onto the stack.
  | Reduce (Production () nts sts ())       -- ^ Reduce a production rule (and fire off any data constructor)
  | Accept                                  -- ^ The parser has accepted the input.
  | Error                                   -- ^ A parse error occured.
  deriving (Generic, Eq, Ord, Hashable, Show, Data, Lift)

-- | An LR configurate telling you the current stack of states @[lrstate]@,
--   and the rest of the input tokens @[t]@.
type Config lrstate t = ([lrstate], [t])

-- | The different kinds of results an LR parser can return.
data LRResult lrstate c t ast =
    ErrorNoAction t (Config lrstate c) [ast]        -- ^ Parser got stuck (no action performable).
  | ErrorAccept     (Config lrstate c) [ast]        -- ^ Parser accepted but still has @ast@s to consume.
  | ResultSet     (Set (LRResult lrstate c t ast))  -- ^ The grammar / parse was ambiguously accepted.
  | ResultAccept  ast                               -- ^ Parse accepted and produced a single @ast@.
  | ErrorTable    (Config lrstate c) [ast]          -- ^ The goto table was missing an entry.
  deriving (Eq, Ord, Show, Generic, Hashable)

-- | A tokenizer is a function that, given a set of DFA names to try tokenizing,
--   returns a parsed token @t@ and the remaining untokenized input @[c]@.
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

instance  ( Prettify c, Prettify t, Prettify ast, Prettify lrstate
          , Eq c, Eq t, Eq ast, Eq lrstate
          , Hashable c, Hashable ast, Hashable t, Hashable lrstate)
  => Prettify (LRResult lrstate c t ast) where
  
  prettify (ErrorNoAction t (s:states, ws) asts) = do
    pStr "ErrorNoAction: Current input token = <"
    prettify t
    --if null ws then return () else prettify (head ws)
    pLine ">"
    incrIndent 7
    
    pStr "Current state = <"
    prettify s
    pLine ">"

    pStr "Rest of input = '"
    prettify ws
    pLine "'"
  
  prettify (ErrorTable (s:states, ws) asts) = do
    pStr "ErrorTable: Current input character = <"
    if null ws then return () else prettify (head ws)
    pLine ">"
    incrIndent 7
    
    pStr "Current state = <"
    prettify s
    pLine ">"

    pStr "Rest of input = '"
    prettify ws
    pLine "'"
    
  prettify (ErrorAccept   (s:states, ws) asts) = do
    pStr "ErrorAccept: Current input character = <"
    (if null ws then return () else prettify (head ws))
    pLine ">"
    incrIndent 7
    
    pStr "Current state = "
    prettify s
    pLine ""
    
    pStr "Rest of input = "
    prettify ws
    pLine ""
 
  prettify (ResultSet s) = pStr "ResultSet: " >> prettify s

  prettify (ResultAccept ast)             = pStr "ResultAccept: " >> prettify ast

-- | Algorithm for computing an SLR closure.
slrClosure :: forall nts sts dt. ( CanParse' nts sts )
  => Grammar () nts sts dt -> SLRClosure (CoreSLRState nts sts)
slrClosure g is' = let

    closure' :: SLRClosure (CoreSLRState nts sts)
    closure' _J = let
      add = fromList
            [ Item (ItemNT _B) [] γ ()
            | Item _A α rst@(pe@(NT _B) : β) () <- toList _J
            , not $ null rst
            , isNT pe
            , Production _ (Prod _ γ) _ <- prodsFor g _B
            ]
      in case size $ add \\ _J of
        0 -> _J `union` add
        _ -> closure' $ _J `union` add

  in closure' is'

-- | Algorithm for computing an LR(1) closure.
lr1Closure :: forall nts sts dt. ( Tabular nts, Tabular sts )
  => Grammar () nts sts dt -> Closure (CoreLR1State nts sts)
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
            , Production _ (Prod _ γ) _ <- prodsFor g _B
            , b <- toList $ LL.first g (β ++ tokenToProdElem a)
            ]
      in case size $ add \\ _J of
        0 -> _J `union` add
        _ -> closure' $ _J `union` add

  in closure' is'

-- | fmap over @lrstate@s of a 'LRAction'.
convAction :: (lrstate -> lrstate') -> LRAction nts sts lrstate -> LRAction nts sts lrstate'
convAction fncn (Shift state) = Shift $ fncn state
convAction _ (Reduce p) = Reduce p
convAction _ Accept = Accept
convAction _ Error = Error

-- | fmap over @lrstate@s of a 'LRTable'.
convTable :: ( IsState lrstate, IsState lrstate', Tabular nts, Tabular sts )
  => (lrstate -> lrstate') -> LRTable nts sts lrstate -> LRTable nts sts lrstate'
convTable fncn tbl = M.fromList'
  [ ((fncn state, icon), S.map (convAction fncn) action)
  | ((state, icon), action) <- M.toList tbl
  ]

-- | Convert the states in a 'LRTable' into integers.
convTableInt :: forall lrstate nts sts. ( IsState lrstate, Tabular nts, Tabular sts )
  => LRTable nts sts lrstate -> [lrstate] -> LRTable nts sts Int
convTableInt tbl ss = convTable (convStateInt $ ss) tbl

-- | fmap over @lrstate@s of a 'Goto'.
convGotoStates :: ( IsState lrstate, IsState lrstate', Tabular nts, Tabular sts )
  => (lrstate -> lrstate') -> Goto nts sts lrstate -> Goto nts sts lrstate'
convGotoStates fncn goto = M1.fromList [ ((fncn st0, e), fncn st1) | ((st0, e), st1) <- M1.toList goto ]

-- | Convert the states in a goto to integers.
convGotoStatesInt :: forall lrstate nts sts. ( IsState lrstate, Tabular sts, Tabular nts )
  => Goto nts sts lrstate -> [lrstate] -> Goto nts sts Int
convGotoStatesInt goto ss = convGotoStates (convStateInt ss) goto

-- | Create a function that, given the list of all possible @lrstate@ elements,
--   converts an @lrstate@ into a unique integer.
convStateInt :: forall lrstate. ( IsState lrstate )
  => [lrstate] -> (lrstate -> Int)
convStateInt ss = let
    statemap :: M1.Map lrstate Int
    statemap = M1.fromList $ zip ss [0 .. ]

    fromJust' st Nothing = error $ "woops: " ++ pshow' st
    fromJust' _ (Just x) = x

  in (\st -> fromJust' st (st `M1.lookup` statemap))

-- | Convert a function-based goto to a map-based one once we know the set of
-- all lrstates (sets of items for LR1) and all the production elements
convGoto :: ( IsState lrstate, Ord sts, Ord nts)
  => Grammar () nts sts dt -> Goto' nts sts lrstate -> [lrstate] -> Goto nts sts lrstate
convGoto g goto states = M1.fromList
  [ ((st0, e), goto st0 e)
  | st0 <- states
  , e   <- allProdElems g
  ]

-- | Get a list of all possible production elements (no epsilon) for the given grammar.
allProdElems :: Grammar () nts ts dt -> [ProdElem nts ts]
allProdElems g =
      map NT (S.toList $ ns g)
  ++  map T  (S.toList $ ts g)

allProdElems' :: forall nts ts. (Bounded nts, Bounded ts, Enum nts, Enum ts)
  => [ProdElem nts ts]
allProdElems' =
      map NT ([minBound .. maxBound] :: [nts])
  ++  map T  ([minBound .. maxBound] :: [ts])

-- | Compute the set of states we would go to by traversing the
--   given nonterminal symbol @_X@.
goto :: ( CanParse' nts sts, Ord a, Hashable a )
  => Grammar () nts sts dt -> Closure (CoreLRState a nts sts) -> Goto' nts sts (CoreLRState a nts sts)
goto g closure is _X = closure $ fromList
  [ Item _A (_X : α) β  a
  | Item _A α (_X' : β) a <- toList is
  , _X == _X'
  ]

-- | Goto with an SLR closure, 'slrClosure'.
slrGoto :: forall nts sts dt. ( CanParse' nts sts )
  => Grammar () nts sts dt -> Goto' nts sts (CoreSLRState nts sts)
slrGoto g = goto g (slrClosure g)

-- | Compute all possible LR items for a grammar by iteratively running
--   goto until reaching a fixed point.
items :: forall nts sts dt a. ( CanParse' nts sts, Ord a, Hashable a )
  => Grammar () nts sts dt -> Goto' nts sts (CoreLRState a nts sts) -> CoreLRState a nts sts -> Set (CoreLRState a nts sts)
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

-- | The kernel of a set items, namely the items where the dot is
--   not at the left-most position of the RHS (also excluding the
--   starting symbol).
kernel :: ( Tabular nts, Tabular sts, Ord a, Hashable a )
  => Set (Item a nts sts) -> Set (Item a nts sts)
kernel = let
    kernel' (Item (Init   _) _  _ _) = True
    kernel' (Item (ItemNT _) [] _ _) = False
    kernel' _ = True
  in S.filter kernel'

-- | Generate the set of all possible Items for a given grammar:
allSLRItems :: forall nts sts dt. ( CanParse' nts sts )
  => Grammar () nts sts dt -> Set (SLRItem nts sts)
allSLRItems g = fromList
    [ Item (Init $ s0 g) [] [NT $ s0 g] ()
    , Item (Init $ s0 g) [NT $ s0 g] [] ()
    ]
  `union`
  fromList
    [ Item (ItemNT nts) (reverse $ take n γ) (drop n γ) ()
    | nts <- toList $ ns g
    , Production _ (Prod _ γ) _ <- prodsFor g nts
    , n <- [0..length γ]
    ]

-- | The starting LR state of a grammar.
lrS0 :: ( Tabular nts, Tabular sts, Ord a, Hashable a )
  => a -> Grammar () nts sts dt -> CoreLRState a nts sts
lrS0 a g = singleton $ Item (Init $ s0 g) [] [NT $ s0 g] a

-- | SLR starting state.
slrS0 :: ( Tabular sts, Tabular nts )
  => Grammar () nts sts dt -> CoreLRState () nts sts
slrS0 = lrS0 ()

-- | Compute SLR table with appropriate 'slrGoto' and 'slrClosure'.
slrItems :: forall nts sts dt. (Tabular nts, Tabular sts )
  => Grammar () nts sts dt -> Set (Set (SLRItem nts sts))
slrItems g = items g (slrGoto g) (slrClosure g $ slrS0 g)

-- | Algorithm for computing the SLR table.
slrTable :: forall nts sts dt. ( Tabular nts, Tabular sts )
  => Grammar () nts sts dt -> SLRTable nts sts (CoreSLRState nts sts)
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
                                          [ ((_Ii, a), Reduce (Production nts (Prod Pass $ reverse α) Nothing))
                                          | a <- (toList . LL.follow g) nts
                                          ]
        slr'' (Item (Init nts) α [] ())   = [((_Ii, IconEOF), Accept)]
        slr'' _ = []
      in concat (S.toList $ S.map slr'' _Ii)

  in M.fromList $ concat $ S.map slr' $ slrItems g

-- | Algorithm for computing the LR(1) table.
lr1Table :: forall nts sts dt. ( Tabular nts, Tabular sts )
  => Grammar () nts sts dt -> LRTable nts sts (CoreLR1State nts sts)
lr1Table g = let
    --lr1' :: LR1State nts sts -> LRTable nts sts
    lr1' _Ii = let
        --lr1'' :: LR1Item nts sts -> LRTable nts sts
        lr1'' (Item (ItemNT nts) α (T a:β) _) = --uPIO (prints ("TABLE:", a, slrGoto g _Ii $ T a, _Ii)) `seq`
                  Just ((_Ii, Icon a), Shift $ lr1Goto g _Ii $ T a)
        lr1'' (Item (Init   nts) α (T a:β) _) = Just ((_Ii, Icon a), Shift $ lr1Goto g _Ii $ T a)
        lr1'' (Item (ItemNT nts) α [] a)      = Just ((_Ii,       a), Reduce (Production nts (Prod Pass $ reverse α) Nothing))
        lr1'' (Item (Init nts) α [] IconEOF)  = Just ((_Ii, IconEOF), Accept)
        lr1'' _ = Nothing
      in catMaybes (S.toList $ S.map lr1'' _Ii)

  in M.fromList $ concat (S.map lr1' $ lr1Items g)

-- | Lookup a value in an 'LRTable'.
look :: ( IsState lrstate, Tabular nts, Tabular sts )
  => (lrstate, Icon sts) -> LRTable nts sts lrstate -> Set (LRAction nts sts lrstate)
look (s,a) tbl = --uPIO (prints ("lookup:", s, a, M.lookup (s, a) act)) `seq`
    M.lookup (s, a) tbl

-- | Is the 'LRResult' an accept?
isAccept (ResultAccept _) = True
isAccept _                = False

-- | Is this 'LRResult' an error?
isError (ResultAccept _) = False
isError _                = True

-- | Get just the LR results which accepted.
getAccepts xs = fromList [x | x <- toList xs, isAccept x]

-- | The core LR parsing algorithm, parametrized for different variants
--   (SLR, LR(1), ...).
lrParse ::
  forall nts t dt ast lrstate.
  ( CanParse nts t, IsState lrstate, IsAST ast )
  => Grammar () nts (StripEOF (Sym t)) dt -> LRTable nts (StripEOF (Sym t)) lrstate -> Goto nts (StripEOF (Sym t)) lrstate
  -> lrstate -> Action ast nts t
  -> [t] -> LRResult lrstate t t ast
lrParse g tbl goto s_0 act w = let
  
    lr :: Config lrstate t -> [ast] -> LRResult lrstate t t ast
    lr (s:states, a:ws) asts = let
        
        lr' :: LRAction nts (StripEOF (Sym t)) lrstate -> LRResult lrstate t t ast
        lr' Accept = case length asts of
              1 -> ResultAccept $ head asts
              _ -> ErrorAccept (s:states, a:ws) asts
        lr' Error     = ErrorNoAction a (s:states, ws) asts
        lr' (Shift t) = trace ("Shift: " ++ pshow' t) $ lr (t:s:states, ws) $ act (TermE a) : asts
        lr' (Reduce p@(Production _A (Prod _ β) _)) = let
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
          then ErrorNoAction a (s:states, ws) asts
          else lr' $ (head . S.toList) lookVal

  in lr ([s_0], w) []

-- | Entrypoint for SLR parsing.
slrParse ::
  forall nts t dt ast. ( CanParse nts t, IsAST ast )
  => Grammar () nts (StripEOF (Sym t)) dt -> Action ast nts t -> [t]
  -> LRResult (CoreSLRState nts (StripEOF (Sym t))) t t ast
slrParse g = lrParse g (slrTable g) (convGoto g (slrGoto g) (sort $ S.toList $ slrItems g)) (slrClosure g $ slrS0 g)

-- | SLR language recognizer.
slrRecognize :: forall nts t dt. ( CanParse nts t )
  => Grammar () nts (StripEOF (Sym t)) dt -> [t] -> Bool
slrRecognize g w = isAccept $ slrParse g (const ()) w

-- | LR(1) language recognizer.
lr1Recognize :: forall nts t dt. ( CanParse nts t )
  => Grammar () nts (StripEOF (Sym t)) dt -> [t] -> Bool
lr1Recognize g w = isAccept $ lr1Parse g (const ()) w

-- | Get just the lookahead symbols for a set of LR(1) items.
getLookAheads :: ( Tabular sts, Tabular nts ) => Set (LR1Item nts sts) -> Set sts
getLookAheads = let
    gLA (Item _ _ _ IconEOF)    = Nothing
    gLA (Item _ _ _ (Icon sts)) = Just sts
  in S.fromList . catMaybes . S.toList . S.map gLA

-- | LR(1) goto table (function) of a grammar.
lr1Goto :: ( Tabular nts, Tabular sts )
  => Grammar () nts sts dt -> Goto' nts sts (CoreLR1State nts sts)
lr1Goto g = goto g (lr1Closure g)

-- | LR(1) start state of a grammar.
lr1S0 :: ( Tabular sts, Tabular nts )
  => Grammar () nts sts dt -> CoreLRState (LR1LookAhead sts) nts sts
lr1S0 = lrS0 IconEOF

-- | Items computed for LR(1) with an 'lr1Goto' and an 'lr1Closure'.
lr1Items :: ( CanParse' nts sts )
  => Grammar () nts sts dt -> Set (CoreLRState (LR1LookAhead sts) nts sts)
lr1Items g = items g (lr1Goto g) (lr1Closure g $ lr1S0 g)

-- | Entrypoint for LR(1) parser.
lr1Parse :: forall nts t dt ast. ( CanParse nts t, IsAST ast )
  => Grammar () nts (StripEOF (Sym t)) dt -> Action ast nts t -> [t]
  -> LRResult (CoreLR1State nts (StripEOF (Sym t))) t t ast
lr1Parse g = lrParse g (lr1Table g) (convGoto g (lr1Goto g) (sort $ S.toList $ lr1Items g)) (lr1Closure g $ lr1S0 g)

-- | Non-incremental GLR parsing algorithm.
glrParse' ::
  forall nts t dt ast lrstate.
  ( CanParse nts t, IsState lrstate, IsAST ast )
  => Grammar () nts (StripEOF (Sym t)) dt -> LRTable nts (StripEOF (Sym t)) lrstate -> Goto nts (StripEOF (Sym t)) lrstate
  -> lrstate -> Action ast nts t
  -> [t] -> LRResult lrstate t t ast
glrParse' g tbl goto s_0 act w = let
  
    lr :: Config lrstate t -> [ast] -> LRResult lrstate t t ast
    lr (s:states, a:ws) asts = let
        
        lr' :: LRAction nts (StripEOF (Sym t)) lrstate -> LRResult lrstate t t ast
        lr' Accept    = case length asts of
              1 -> ResultAccept $ head asts
              _ -> ErrorAccept (s:states, a:ws) asts
        lr' Error     = ErrorNoAction a (s:states, ws) asts
        lr' (Shift t) = trace ("Shift: " ++ pshow' t) $ lr (t:s:states, ws) $ act (TermE a) : asts
        lr' (Reduce p@(Production _A (Prod _ β) _)) = let
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
          then ErrorNoAction a (s:states, ws) asts
          else (if S.null justAccepts
                  then (case S.size parseResults of
                          0 -> undefined
                          1 -> S.findMin parseResults
                          _ -> ResultSet parseResults)
                  else ResultSet justAccepts)

  in lr ([s_0], w) []

-- | Entrypoint for GLR parsing algorithm.
glrParse g = glrParse' g (lr1Table g) (convGoto g (lr1Goto g) (sort $ S.toList $ lr1Items g)) (lr1Closure g $ lr1S0 g)

glrParseInc' ::
  forall nts t dt ast lrstate c.
  ( CanParse nts t, IsState lrstate, IsAST ast, Tabular c )
  => Grammar () nts (StripEOF (Sym t)) dt -> LRTable nts (StripEOF (Sym t)) lrstate -> Goto nts (StripEOF (Sym t)) lrstate
  -> lrstate -> M1.Map lrstate (Set (StripEOF (Sym t))) -> Action ast nts t
  -> Tokenizer t c -> [c] -> GLRResult lrstate c t ast
glrParseInc' g tbl goto s_0 tokenizerFirstSets act tokenizer w = let
    
    lr :: Config lrstate c -> [ast] -> GLRResult lrstate c t ast
    lr (s:states, cs) asts = let

        -- The set of token symbols that are feasible to be seen next given the
        -- current grammar context - i.e. the Set of LR1LookAheads stripped from
        -- the current state on top of the configuration stack. Luckily enough,
        -- it just so happens that the type stuffed inside an LR1 lookahead Icon
        -- is precisely the terminal symbol type that the tokenizer uses to name
        -- DFAs.
        dfaNames = fromMaybe (error $ "Failed DFA lookup: " ++ pshow' (s, tokenizerFirstSets))
          $ s `M1.lookup` tokenizerFirstSets
        
        (a, ws) = tokenizer dfaNames cs
        
        lr' :: LR1Action nts (StripEOF (Sym t)) lrstate -> GLRResult lrstate c t ast
        lr' Accept    = case length asts of
              1 -> ResultAccept $ head asts
              _ -> ErrorAccept (s:states, cs) asts
        lr' Error     = ErrorNoAction a (s:states, cs) asts
        lr' (Shift t) = trace ("Shift: " ++ pshow' t) $ lr (t:s:states, ws) $ act (TermE a) : asts
        lr' (Reduce p@(Production _A (Prod _ β) _)) = let
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
          then ErrorNoAction a (s:states, cs) asts
          else (if S.null justAccepts
                  then (case S.size parseResults of
                          0 -> undefined
                          1 -> S.findMin parseResults
                          _ -> ResultSet parseResults)
                  else (case S.size justAccepts of
                          1 -> S.findMin justAccepts
                          _ -> ResultSet justAccepts))

  in lr ([s_0], w) []

-- | Mapping from parse states to which symbols can be seen next so that the
--   incremental tokenizer can check which DFAs to try tokenizing.
tokenizerFirstSets convState g = let
    tbl = lr1Table g

    first s = let
        removeIcons (Icon t) = Just t
        removeIcons IconEps  = Nothing
        removeIcons IconEOF  = Nothing

        itemHeads (Item (Init   nt) _ [] _) = []
        itemHeads (Item (ItemNT nt) _ [] _) = S.toList $ LL.follow g nt -- TODO: Use stack context
        itemHeads (Item _ _ (b:bs)  _)      = S.toList $ LL.first  g [b]

      in S.fromList $ mapMaybe removeIcons $ concatMap itemHeads s

  in M1.fromList [ (convState s, first $ S.toList s) | ((s, _), _) <- M.toList tbl ]

-- | Entrypoint for an incremental GLR parser.
glrParseInc g = glrParseInc' g
  (lr1Table g)
  (convGoto g (lr1Goto g) (sort $ S.toList $ lr1Items g))
  (lr1Closure g $ lr1S0 g)
  (tokenizerFirstSets id g)

-- | Incremental GLR parser with parse states compressed into integers.
glrParseInc2 g = let
    is = sort $ S.toList $ lr1Items g
    convState = convStateInt is
  in glrParseInc' g
      (convTableInt (lr1Table g) is)
      (convGotoStatesInt (convGoto g (lr1Goto g) is) is)
      (convState $ lr1Closure g $ lr1S0 g)
      (tokenizerFirstSets convState g)

-- | Returns the disambiguated LRTable, as well as the number of conflicts
--   (Shift/Reduce, Reduce/Reduce, etc...) reported.
disambiguate ::
  ( IsState lrstate, Tabular nts, Tabular sts
  , Data lrstate, Data nts, Data sts)
  => LRTable nts sts lrstate -> (LRTable' nts sts lrstate, Int)
disambiguate tbl = let

    mkConflict s = concatWith "/" $ map (show . toConstr) $ S.toList s

    mkSingle st icon s
      | S.size s == 1 = (S.findMin s, 0)
      | S.size s == 0 = D.trace ("Table entry " ++ pshow' (st,icon) ++ " has no Shift/Reduce entry.") undefined
      | otherwise     = D.trace ("Table entry " ++ pshow' (st,icon) ++ " has " ++ mkConflict s ++ " conflict: \n"
                        ++  (pshow' $ S.toList s)) (S.findMin s, 1)
  in (M1.fromList
    [ ((st, icon), fst (mkSingle st icon action))
    | ((st, icon), action) <- M.toList tbl
    ], sum
    [ snd (mkSingle st icon action)
    | ((st, icon), action) <- M.toList tbl
    ])
