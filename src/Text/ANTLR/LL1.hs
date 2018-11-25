{-# LANGUAGE ScopedTypeVariables, MonadComprehensions, DeriveGeneric
 , DeriveAnyClass, FlexibleContexts, OverloadedStrings #-}
{-|
  Module      : Text.ANTLR.LL1
  Description : LL1 parsing algorithm and accompanying first/follow functions
  Copyright   : (c) Karl Cronburg, 2018
  License     : BSD3
  Maintainer  : karl@cs.tufts.edu
  Stability   : experimental
  Portability : POSIX

-}
module Text.ANTLR.LL1
  ( recognize
  , first, follow
  , foldWhileEpsilon
  , isLL1, parseTable
  , predictiveParse
  , removeEpsilons, removeEpsilons'
  , leftFactor
  , Prime(..)
  ) where
import Text.ANTLR.Grammar
import Text.ANTLR.Pretty
import Text.ANTLR.Parser
import Text.ANTLR.Allstar.ATN
--import Data.Set.Monad
import Text.ANTLR.Set
  ( Set(..), singleton, fromList, union, empty, member, size, toList
  , insert, delete, intersection, Hashable(..), Generic(..), maybeMin
  )
import Data.List (maximumBy, isPrefixOf)
import Data.Ord (comparing)

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Debug.Trace as D
import System.IO.Unsafe (unsafePerformIO)
uPIO = unsafePerformIO

-- Fold while the given pred function is true:
foldWhile :: (a -> b -> Bool) -> (a -> b -> b) -> b -> [a] -> b
foldWhile pred fncn = let
    fW' b0 []     = b0
    fW' b0 [a]    = b0
    fW' b0 (a:as)
      | pred a b0 = fW' (fncn a b0) as
      | otherwise = b0
  in fW'

epsIn set _ = IconEps `member` set

-- | Fold over a set of ProdElems (symbols) while all the previous sets of
--   symbols contains an epsilon.
foldWhileEpsilon fncn b0 []     = empty
foldWhileEpsilon fncn b0 [a]    = fncn a b0
foldWhileEpsilon fncn b0 (a:as)
  | epsIn a b0 = foldWhile epsIn fncn (fncn a b0) as
  | otherwise  = fncn a b0

-- | First set of a grammar.
first ::
  forall sts nts. (Eq nts, Eq sts, Ord nts, Ord sts, Hashable nts, Hashable sts)
  => Grammar () nts sts -> [ProdElem nts sts] -> Set (Icon sts)
first g = let
    firstOne :: Set (ProdElem nts sts) -> ProdElem nts sts -> Set (Icon sts)
    firstOne _ t@(T x) = singleton $ Icon x
    firstOne _ Eps     = singleton IconEps
    firstOne busy nts@(NT x)
      | nts `member` busy = empty
      | otherwise = foldr union empty
            [ foldWhileEpsilon union empty
              [ firstOne (insert nts busy) y
              | y <- (\(Prod _ ss) -> ss) rhs
              ]
            | Production _ rhs <- prodsFor g x ]
    
    firstMany :: [Set (Icon sts)] -> Set (Icon sts)
    firstMany []   = singleton IconEps
    firstMany (ts:tss)
      | IconEps `member` ts = ts `union` firstMany tss
      | otherwise           = ts
  in firstMany . map (firstOne empty)

-- | Follow set of a grammar.
follow ::
  forall nts sts. (Eq nts, Eq sts, Ord nts, Ord sts, Hashable nts, Hashable sts)
  => Grammar () nts sts -> nts -> Set (Icon sts)
follow g = let
    follow' busy _B
      | _B `member` busy = empty
      | otherwise = let

        busy' = insert _B busy
        
        followProd :: nts -> ProdElems nts sts -> Set (Icon sts)
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
              (delete IconEps $ first g β)
              `union`
              -- If A -> αBβ and Epsilon `member` FIRST(β), then everything
              -- in FOLLOW(A) is in FOLLOW(B)
              (if IconEps `member` first g β
                then follow' busy' _A
                else empty
              )

            -- Start state contains IconEOF (aka '$', end of input) in FOLLOW()
      in  (if _B == s0 g then singleton IconEOF else empty)
          `union`
          foldr union empty
            [ followProd lhs_nts ss
            | Production lhs_nts (Prod _ ss) <- ps g
            ]
  in follow' empty

-- | Is the given grammar in LL(1)?
--   
-- @
--   A -> α | β for all distinct ordered pairs of α and β,
--        first(α) `intersection` first(β) == empty
--   and if epsilon is in α, then
--        first(α) `intersection` follow(A) == empty
-- @
isLL1
  :: (Eq nts, Eq sts, Ord nts, Ord sts, Hashable nts, Hashable sts)
  => Grammar () nts sts -> Bool
isLL1 g =
  validGrammar g && and
      [  (first g α `intersection` first  g β  == empty)
      && (not (IconEps `member` first g α)
         || ((first g α `intersection` follow g nts) == empty))
      | nts       <- toList $ ns g
      , (Prod _ α) <- map getRHS $ prodsFor g nts
      , (Prod _ β) <- map getRHS $ prodsFor g nts
      , α /= β
      ]

type Key nts sts = (nts, Icon sts)

-- All possible productions we could reduce. Empty implies parse error,
-- singleton implies unambiguous entry, multiple implies ambiguous:
type Value nts sts = Set (ProdElems nts sts)

ambigVal
  :: (Ord nts, Ord sts, Hashable nts, Hashable sts)
  => Value nts sts -> Bool
ambigVal = (1 >) . size

-- M[A,s] = α for each symbol s `member` FIRST(α)
type ParseTable nts sts = M.Map (Key nts sts) (Value nts sts)

parseTable' ::
  forall nts sts. (Eq nts, Eq sts, Ord nts, Ord sts, Eq nts, Hashable sts, Hashable nts)
  => (Value nts sts -> Value nts sts -> Value nts sts) -> Grammar () nts sts -> ParseTable nts sts
parseTable' fncn g = let

    insertMe ::
      (nts, Icon sts, ProdElems nts sts) -> (ParseTable nts sts -> ParseTable nts sts)
    insertMe (_A, a, α) = M.insertWith fncn (_A, a) $ singleton α

  in
    foldr insertMe M.empty
      -- For each terminal a `member` FIRST(α), add A -> α to M[A,α]
      [ (_A, Icon a, α)
      | Production _A (Prod _ α) <- ps g
      , Icon a <- toList $ first g α
      ]
    `M.union`
    foldr insertMe M.empty
      -- If Eps `member` FIRST(α), add A -> α to M[A,b]
      -- for each b `member` FOLLOW(A)
      [ (_A, Icon b, α)
      | Production _A (Prod _ α) <- ps g
      , IconEps `member` first g α
      , Icon b <- toList $ follow g _A
      ]
    `M.union`
    foldr insertMe M.empty
      -- If Eps `member` FIRST(α)
      -- , AND IconEOF `member` FOLLOW(_A)
      -- add A -> α to M[A,IconEOF]
      [ (_A, IconEOF, α)
      | Production _A (Prod _ α) <- ps g
      , IconEps `member` first g α
      , IconEOF  `member` follow g _A
      ]

-- | The algorithm for computing an LL parse table from a grammar.
parseTable :: 
  forall nts sts. (Eq nts, Eq sts, Ord nts, Ord sts, Hashable sts, Hashable nts)
  => Grammar () nts sts -> ParseTable nts sts
parseTable = parseTable' union


data TreeNode ast nts sts =
    Comp   ast
  | InComp nts (ProdElems nts sts) [ast] Int
  deriving (Eq, Ord, Show)

instance (Prettify ast, Prettify nts, Prettify sts) => Prettify (TreeNode ast nts sts) where
  prettify (Comp ast)            = do
    pStr "(Complete "
    prettify ast
    pStr ")"
  prettify (InComp nts es asts i) = pParens $ do
    pStr "InComp"
    incrIndent 2
    pLine ""
    pStr "nts="
    prettify nts
    pLine ""
    pStr "es="
    prettify es
    pLine ""
    pStr "asts="
    prettify asts
    pLine ""
    pStr "i="
    prettify i
    incrIndent (-2)

-- A stack tree is a list of tree nodes with terminal *tokens* (not terminal
-- symbols)
type StackTree ast nts ts = [TreeNode ast nts (StripEOF ts)]

isComp (Comp _) = True
isComp _ = False
isInComp = not . isComp

-- | Language recognizer using 'predictiveParse'.
recognize ::
  ( Eq nts, Ref t, Eq (Sym t), HasEOF (Sym t)
  , Ord nts, Ord t, Ord (Sym t), Ord (StripEOF (Sym t))
  , Prettify nts, Prettify t, Prettify (Sym t), Prettify (StripEOF (Sym t))
  , Hashable (Sym t), Hashable nts, Hashable (StripEOF (Sym t)))
  => Grammar () nts (StripEOF (Sym t)) -> [t] -> Bool
recognize g = (Nothing /=) . predictiveParse g (const ())

-- | Top-down predictive parsing algorithm.
predictiveParse
  :: forall nts t ast.
  (Prettify nts, Prettify t, Prettify (Sym t), Prettify (StripEOF (Sym t)), Prettify ast
  , Eq nts, Eq (Sym t)
  , HasEOF (Sym t)
  , Ord (Sym t), Ord nts, Ord t, Ord (StripEOF (Sym t))
  , Hashable (Sym t), Hashable nts, Hashable (StripEOF (Sym t))
  , Ref t)
  =>  Grammar () nts (StripEOF (Sym t)) -> Action ast nts t -> [t] ->  Maybe ast
predictiveParse g act w0 = let

    --reduce :: StackTree ast -> StackTree ast
    reduce :: StackTree ast nts (Sym t) -> StackTree ast nts (Sym t)
    reduce stree@(InComp nts ss asts 0 : rst) = reduce $ Comp (act $ NonTE (nts, ss, reverse asts)) : rst
    reduce stree@(InComp{}:_) = stree
    reduce stree = let
        
        cmps = map (\(Comp ast) -> ast) $ takeWhile isComp stree
        (InComp nts ss asts i : rst) = dropWhile isComp stree
        -- @(InComp nts ss ast i:rst) = dropWhile isComp stree
        
      in case dropWhile isComp stree of
            []                          -> stree
            (InComp nts ss asts i : rst) -> reduce (InComp nts ss (cmps ++ asts) (i - length cmps) : rst)
      
    -- Push a production elements (NT, T, or Eps) onto a possibly incomplete
    -- stack of trees
    --pushStack :: ProdElem -> ProdElems -> StackTree ast -> StackTree ast
    pushStack :: ProdElem nts t -> ProdElems nts (StripEOF (Sym t)) -> StackTree ast nts (Sym t) -> StackTree ast nts (Sym t)
    pushStack (NT nts) ss stree = reduce $ InComp nts ss [] (length ss) : stree
    pushStack (T t)   _  (InComp nts ss asts i:stree) = reduce $ InComp nts ss (act (TermE t) : asts) (i - 1) : stree
    pushStack Eps     _  (InComp nts ss asts i:stree) = reduce $ InComp nts ss (act EpsE             : asts) (i - 1) : stree
   
    -- ParseTable terminal type *has* an EOF (not StripEOF (Sym t))
    _M :: ParseTable nts (StripEOF (Sym t))
    _M = parseTable g

    -- input word LL1 symbols -> Stack of symbols -> AST
    -- [ast] - a stack (list) of the asts the user has computed for us
    --         intermixed (in proper order) with the Terminals in the production
    --         rule for which we reduced the NonTerminal in question.
    parse' :: [t] -> ProdElems nts (StripEOF (Sym t)) -> StackTree ast nts (Sym t) -> Maybe (StackTree ast nts (Sym t)) --Maybe ast
    parse' [] [] asts                        = Just asts  -- Success? (TODO - EOF assumed on empty input)
    parse' [t] [] asts | isEOF $ getSymbol t = Just asts  -- Success!
    parse' _   [] asts  = Nothing    -- Parse failure because no end of input found
    parse' (a:ws) (T x:xs) asts
      | stripEOF (getSymbol a) == Just x = parse' ws xs $ pushStack (T a) [] asts
      | otherwise  = Nothing
    parse' ws@(a:_) (NT _X:xs) asts = do
      let sym  = getSymbol a
      sym' <- if isEOF sym then Just IconEOF else Icon <$> stripEOF (getSymbol a)
      ss  <- (_X, sym') `M.lookup` _M
      --D.traceM $ "ss=" ++ pshow ss
      ss' <- maybeMin ss
      --D.traceM $ "ss'=" ++ pshow ss'
      parse' ws (ss' ++ xs) (pushStack (NT _X) ss' asts)
    parse' ws (Eps:xs) asts = parse' ws xs (pushStack Eps [] asts)
    parse' ws xs asts = D.trace (T.unpack $ "Bug in parser: " `T.append` pshow (ws, xs, asts)) Nothing -- Bug in parser

  in do asts <- parse' w0 [NT $ s0 g] []
        case asts of
          [Comp ast] -> Just ast
          _          -> Nothing

-- | Remove all epsilon productions, i.e. productions of the form "A -> eps",
--   without affecting the language accepted.
removeEpsilons' ::
  forall s nts t. (Eq t, Eq nts, Prettify t, Prettify nts, Prettify s, Ord t, Ord nts, Hashable t, Hashable nts)
  => [Production s nts t] -> [Production s nts t]
removeEpsilons' ps_init = let

    epsNT :: Production s nts t -> [nts] -> [nts]
    epsNT (Production nts (Prod _ []))    = (:) nts
    epsNT (Production nts (Prod _ [Eps])) = (:) nts
    epsNT prod             = id
  
    -- All NTs with an epsilon production
    epsNTs :: [nts]
    epsNTs = foldr epsNT [] ps_init

    {-
    isEpsProd :: Production s nts t -> Bool
    isEpsProd []         = True
    isEpsProd [Prod Eps] = True
    isEPsProd _          = False
    -}

    replicateProd :: nts -> Production s nts t -> [Production s nts t]
    replicateProd nts0 (Production nt1 (Prod sf es)) = let
        
        rP :: ProdElems nts t -> ProdElems nts t -> [Production s nts t]
        rP ys []   = [Production nt1 (Prod sf $ reverse ys)]
        rP ys (x:xs)
          | NT nts0 == x
              = Production nt1 (Prod sf (reverse ys ++ xs))   -- Production with nts0 removed
              : Production nt1 (Prod sf (reverse ys ++ x:xs)) -- Production without nts0 removed
              : (  rP ys     xs  -- Recursively with nts0 removed
                ++ rP (x:ys) xs) -- Recursively without nts0 removed
          | otherwise = rP (x:ys) xs
      in rP [] es

    orderNub ps p1
      | p1 `elem` ps = ps
      | otherwise    = p1 : ps

    ps' :: [Production s nts t]
    ps' = case epsNTs of
      []         -> ps_init
      (nts:ntss) -> removeEpsilons' $
                    foldl orderNub []
                          [ p' 
                          | p  <- ps_init
                          , p' <- replicateProd nts p
                          , p' /= Production nts (Prod Pass [])
                          , p' /= Production nts (Prod Pass [Eps])]

  in ps'

-- | Remove all epsilon productions, i.e. productions of the form "A -> eps",
--   without affecting the language accepted.
removeEpsilons ::
  forall s nts t. (Eq t, Eq nts, Prettify t, Prettify nts, Prettify s, Ord t, Ord nts, Hashable t, Hashable nts)
  => Grammar s nts t -> Grammar s nts t
removeEpsilons g = g { ps = removeEpsilons' $ ps g }

newtype Prime nts = Prime (nts, Int)
  deriving (Eq, Ord, Generic, Hashable, Show)

instance (Prettify nts) => Prettify (Prime nts) where
  prettify (Prime (nts,i)) = do
    prettify nts
    pStr $ T.replicate i (T.singleton '\'')

-- | Left-factor a grammar to make it LL(1). This is experimental and mostly untested.
--   This adds 'Prime's to the nonterminal symbols in cases where we need to break up
--   a production rule in order to left factor it.
leftFactor ::
  forall s nts t. (Eq t, Eq nts, Prettify t, Prettify nts, Ord t, Ord nts, Hashable nts)
  => Grammar s nts t -> Grammar s (Prime nts) t
leftFactor = let

  primeify :: Grammar s nts t -> Grammar s (Prime nts) t
  primeify g = G
    { ns = fromList $ [ Prime (nts, 0) | nts <- toList $ ns g ]
    , ts = ts g
    , ps = [ Production (Prime (nts, 0)) (Prod sf $ map prmPE ss)
           | Production nts (Prod sf ss) <- ps g ]
    , s0 = Prime (s0 g, 0)
    , _πs = _πs g
    , _μs = _μs g
    }

  prmPE :: ProdElem nts t -> ProdElem (Prime nts) t
  prmPE (NT nts) = NT $ Prime (nts, 0)
  prmPE (T x)   = T x
  prmPE Eps     = Eps
  
  lF :: Grammar s (Prime nts) t -> Grammar s (Prime nts) t
  lF g = let
    -- Longest common prefix of two lists
    lcp :: ProdElems (Prime nts) t -> ProdElems (Prime nts) t -> ProdElems (Prime nts) t
    lcp [] ys = []
    lcp xs [] = []
    lcp (x:xs) (y:ys)
      | x == y    = x : lcp xs ys
      | otherwise = []

    lcps :: [(Prime nts, ProdElems (Prime nts) t)]
    lcps = [ (nts0, maximumBy (comparing length)
                   [ lcp xs ys
                   | Production _ (Prod _ xs) <- filter ((== nts0) . getLHS) (ps g)
                   , Production _ (Prod _ ys) <- filter ((== nts0) . getLHS) (ps g)
                   , xs /= ys
                   ])
           | nts0 <- toList $ ns g ]

    --longest_lcps :: [(nts, ProdElems nts t)]
    --longest_lcps = filter (not . null . snd) lcps

    incr :: Prime nts -> Prime nts
    incr (Prime (nts, i)) = Prime (nts, i + 1)

    ps' :: [(Prime nts, ProdElems (Prime nts) t)] -> [Production s (Prime nts) t]
    ps' []           = ps g
    ps' ((nts, xs):_) =
        -- Unaffected productions
        [ Production nts0 (Prod v rhs)
        | Production nts0 (Prod v rhs) <- ps g
        , nts0 /= nts
        ]
      ++
        -- Unaffected productions
        [ Production nts0 (Prod v rhs)
        | Production nts0 (Prod v rhs) <- ps g
        , nts == nts0 && not (xs `isPrefixOf` rhs)
        ]
      ++
        -- Affected productions
        [ Production (incr nts0) (Prod v (drop (length xs) rhs))
        | Production nts0 (Prod v rhs) <- ps g
        , nts == nts0 && xs `isPrefixOf` rhs
        ]
      ++ [Production nts (Prod Pass $ xs ++ [NT $ incr nts])]
  {- [ (prime nts, drop (length xs) ys)
                    | (nt1, ys) <- ps g
                    , nt1 == nts
                    , xs `isPrefixOf` ys -}
                    
    in g { ps = ps' lcps }
  in lF . primeify

