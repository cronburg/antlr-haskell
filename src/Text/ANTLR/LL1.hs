{-# LANGUAGE ScopedTypeVariables, MonadComprehensions, DeriveGeneric
 , DeriveAnyClass, FlexibleContexts #-}
module Text.ANTLR.LL1
  ( recognize
  , first, follow
  , foldWhileEpsilon
  , isLL1, parseTable
  , predictiveParse
  , removeEpsilons
  , leftFactor, Prime(..)
  ) where
import Text.ANTLR.Allstar.Grammar
import Text.ANTLR.Pretty
import Text.ANTLR.Parser
import Text.ANTLR.Allstar.ATN
--import Data.Set.Monad
import Text.ANTLR.Set
  ( Set(..), singleton, fromList, union, empty, member, size, toList
  , insert, delete, intersection, Hashable(..), Generic(..), findMin
  )
import Data.List (maximumBy, isPrefixOf)
import Data.Ord (comparing)

import qualified Data.Map.Strict as M

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

-- Fold over a set of ProdElems (symbols) while all the previous sets of
-- symbols contains an epsilon.
foldWhileEpsilon fncn b0 []     = empty
foldWhileEpsilon fncn b0 [a]    = fncn a b0
foldWhileEpsilon fncn b0 (a:as)
  | epsIn a b0 = foldWhile epsIn fncn (fncn a b0) as
  | otherwise  = fncn a b0

first ::
  forall t nt. (Ref nt, Eq (Sym nt), Ref t, Eq (Sym t), Ord nt, Ord t, Hashable nt, Hashable t)
  => Grammar () nt t -> [ProdElem nt t] -> Set (Icon t)
first g = let
    firstOne :: Set (ProdElem nt t) -> ProdElem nt t -> Set (Icon t)
    firstOne _ t@(T x) = singleton $ Icon x
    firstOne _ Eps     = singleton IconEps
    firstOne busy nt@(NT x)
      | nt `member` busy = empty
      | otherwise = foldr union empty
            [ foldWhileEpsilon union empty
              [ firstOne (insert nt busy) y
              | y <- (\(Prod _ ss) -> ss) rhs
              ]
            | Production _ rhs <- prodsFor g x ]
    
    firstMany :: [Set (Icon t)] -> Set (Icon t)
    firstMany []   = singleton IconEps
    firstMany (ts:tss)
      | IconEps `member` ts = ts `union` firstMany tss
      | otherwise           = ts
  in firstMany . map (firstOne empty)

follow ::
  forall nt t. (Ref nt, Eq (Sym nt), Ref t, Eq (Sym t), Ord nt, Ord t, Hashable nt, Hashable t)
  => Grammar () nt t -> nt -> Set (Icon t)
follow g = let
    follow' busy _B
      | _B `member` busy = empty
      | otherwise = let

        busy' = insert _B busy
        
        followProd :: nt -> ProdElems nt t -> Set (Icon t)
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
            [ followProd lhs_nt ss
            | Production lhs_nt (Prod _ ss) <- ps g
            ]
  in follow' empty

-- A -> α | β for all distinct ordered pairs of α and β,
--      first(α) `intersection` first(β) == empty
-- and if epsilon is in α, then
--      first(α) `intersection` follow(A) == empty
isLL1
  :: (Ref nt, Eq (Sym nt), Ref t, Eq (Sym t), Eq nt, Ord nt, Eq t, Ord t, Hashable nt, Hashable t)
  => Grammar () nt t -> Bool
isLL1 g =
  validGrammar g && and
      [  (first g α `intersection` first  g β  == empty)
      && (not (IconEps `member` first g α)
         || ((first g α `intersection` follow g nt) == empty))
      | nt       <- toList $ ns g
      , (Prod _ α) <- map getRHS $ prodsFor g nt
      , (Prod _ β) <- map getRHS $ prodsFor g nt
      , α /= β
      ]

type Key nt t = (nt, Icon t)

-- All possible productions we could reduce. Empty implies parse error,
-- singleton implies unambiguous entry, multiple implies ambiguous:
type Value nt t = Set (ProdElems nt t)

ambigVal
  :: (Ord nt, Ord t, Hashable nt, Hashable t)
  => Value nt t -> Bool
ambigVal = (1 >) . size

-- M[A,s] = α for each symbol s `member` FIRST(α)
type ParseTable nt t = M.Map (Key nt t) (Value nt t)

parseTable' ::
  forall nt t. (Ref nt, Eq (Sym nt), Ref t, Eq (Sym t), Ord nt, Ord t, Eq t, Eq nt, Hashable t, Hashable nt)
  => (Value nt t -> Value nt t -> Value nt t) -> Grammar () nt t-> ParseTable nt t
parseTable' fncn g = let

    insertMe ::
      (nt, Icon t, ProdElems nt t) -> (ParseTable nt t -> ParseTable nt t)
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

parseTable :: 
  forall nt. forall t. (Ref nt, Eq (Sym nt), Ref t, Eq (Sym t), Ord nt, Ord t, Eq t, Eq nt, Hashable t, Hashable nt)
  => Grammar () nt t -> ParseTable nt t
parseTable = parseTable' union

data TreeNode ast nt t =
    Comp   ast
  | InComp nt (ProdElems nt t) [ast] Int
  deriving (Eq, Ord, Show)

instance (Prettify ast, Prettify nt, Prettify t) => Prettify (TreeNode ast nt t) where
  prettify (Comp ast)            = do
    pStr "(Complete "
    prettify ast
    pStr ")"
  prettify (InComp nt es asts i) = pParens $ do
    pStr "Incomplete "
    prettify nt
    pStr " "
    prettify es
    pStr " "
    prettify asts
    pStr " "
    prettify i

type StackTree ast nt t = [TreeNode ast nt t]

isComp (Comp _) = True
isComp _ = False
isInComp = not . isComp

recognize ::
  (Ref nt, Eq (Sym nt), Ref t, Eq (Sym t), Ord nt, Ord t, Prettify nt, Prettify t, Hashable t, Hashable nt)
  => Grammar () nt t -> [Icon t] -> Bool
recognize g = (Nothing /=) . predictiveParse g (const ())

predictiveParse
  :: forall nt t ast.
  (Prettify nt, Prettify t, Prettify ast, Ref nt, Eq (Sym nt), Ref t, Eq (Sym t), Ord nt, Ord t, Hashable t, Hashable nt)
  => Grammar () nt t -> Action ast nt t -> [Icon t] ->  Maybe ast
predictiveParse g act w0 = let

    --reduce :: StackTree ast -> StackTree ast
    reduce stree@(InComp nt ss asts 0 : rst) = reduce $ Comp (act $ NonTE (nt, ss, reverse asts)) : rst
    reduce stree@(InComp{}:_) = stree
    reduce stree = let
        
        cmps = map (\(Comp ast) -> ast) $ takeWhile isComp stree
        (InComp nt ss asts i : rst) = dropWhile isComp stree
        -- @(InComp nt ss ast i:rst) = dropWhile isComp stree
        
      in case dropWhile isComp stree of
            []                          -> stree
            (InComp nt ss asts i : rst) -> reduce (InComp nt ss (cmps ++ asts) (i - length cmps) : rst)
      
    -- Push a production element (NT, T, or Eps) onto a possibly incomplete
    -- stack of trees
    --pushStack :: ProdElem -> ProdElems -> StackTree ast -> StackTree ast
    pushStack :: ProdElem nt t -> ProdElems nt t -> StackTree ast nt t -> StackTree ast nt t
    pushStack (NT nt) ss stree = reduce $ InComp nt ss [] (length ss) : stree
    pushStack (T t)   _  (InComp nt ss asts i:stree) = reduce $ InComp nt ss (act (TermE (Icon t)) : asts) (i - 1) : stree
    pushStack Eps     _  (InComp nt ss asts i:stree) = reduce $ InComp nt ss (act EpsE             : asts) (i - 1) : stree
    
    _M :: ParseTable nt t
    _M = parseTable g

    -- input word LL1 symbols -> Stack of symbols -> AST
    -- [ast] - a stack (list) of the asts the user has computed for us
    --         intermixed (in proper order) with the Terminals in the production
    --         rule for which we reduced the NonTerminal in question.
    parse' :: [Icon t] -> ProdElems nt t -> StackTree ast nt t -> Maybe (StackTree ast nt t) --Maybe ast
    parse' [IconEOF] [] asts  = Just asts  -- Success!
    parse' _     [] asts  = Nothing    -- Parse failure because no end of input found
    parse' (Icon a:ws) (T x:xs) asts
      | x == a    = parse' ws xs $ pushStack (T x) [] asts
      | otherwise = Nothing
    parse' ws@(a:_) (NT _X:xs) asts =
        case (_X, a) `M.lookup` _M of
          Nothing -> Nothing
          Just ss -> case (size ss, {-0 `elemAt`-} findMin ss) of
              (1,ss') -> parse' ws (ss' ++ xs) (pushStack (NT _X) ss' asts)
              _       -> Nothing
    parse' ws (Eps:xs) asts = parse' ws xs (pushStack Eps [] asts)
    parse' ws xs asts =
      uPIO (putStrLn $ pshow ws ++ pshow xs ++ pshow asts) `seq` undefined

  in do asts <- parse' w0 [NT $ s0 g] []
        case asts of
          [Comp ast] -> Just ast
          _          -> Nothing

{- Remove all epsilon productions, i.e. productions of the form "A -> eps",
 - without affecting the language accepted -}
removeEpsilons ::
  forall s nt t. (Eq t, Eq nt, Prettify t, Prettify nt, Prettify s, Ord t, Ord nt, Hashable t, Hashable nt)
  => Grammar s nt t -> Grammar s nt t
removeEpsilons g = let

    epsNT :: Production s nt t -> [nt] -> [nt]
    epsNT (Production nt (Prod _ []))    = (:) nt
    epsNT (Production nt (Prod _ [Eps])) = (:) nt
    epsNT prod             = id
  
    -- All NTs with an epsilon production
    epsNTs :: [nt]
    epsNTs = foldr epsNT [] (ps g)

    {-
    isEpsProd :: Production s nt t -> Bool
    isEpsProd []         = True
    isEpsProd [Prod Eps] = True
    isEPsProd _          = False
    -}

    replicateProd :: nt -> Production s nt t -> [Production s nt t]
    replicateProd nt0 (Production nt1 (Prod sf es)) = let
        
        rP :: ProdElems nt t -> ProdElems nt t -> [Production s nt t]
        rP ys []   = [Production nt1 (Prod sf $ reverse ys)]
        rP ys (x:xs)
          | NT nt0 == x
              = Production nt1 (Prod sf (reverse ys ++ xs))   -- Production with nt0 removed
              : Production nt1 (Prod sf (reverse ys ++ x:xs)) -- Production without nt0 removed
              : (  rP ys     xs  -- Recursively with nt0 removed
                ++ rP (x:ys) xs) -- Recursively without nt0 removed
          | otherwise = rP (x:ys) xs
      in rP [] es

    orderNub ps p1
      | p1 `elem` ps = ps
      | otherwise    = p1 : ps

    ps' :: [Production s nt t]
    ps' = case epsNTs of
      []       -> ps g
      (nt:nts) -> ps $ removeEpsilons -- $ D.tracePrettifyId
                    (g { ps = foldl orderNub []
                          [ p' 
                          | p  <- ps g
                          , p' <- replicateProd nt p
                          , p' /= Production nt (Prod Pass [])
                          , p' /= Production nt (Prod Pass [Eps])]
                    })

  in g { ps = ps' }

newtype Prime nt = Prime (nt, Int)
  deriving (Eq, Ord, Generic, Hashable, Show)

instance (Prettify nt) => Prettify (Prime nt) where
  prettify (Prime (nt,i)) = do
    prettify nt
    pStr $ replicate i '\''

leftFactor ::
  forall s nt t. (Eq t, Eq nt, Prettify t, Prettify nt, Ord t, Ord nt, Hashable nt)
  => Grammar s nt t -> Grammar s (Prime nt) t
leftFactor = let

  primeify :: Grammar s nt t -> Grammar s (Prime nt) t
  primeify g = G
    { ns = [ Prime (nt, 0) | nt <- ns g ]
    , ts = ts g
    , ps = [ Production (Prime (nt, 0)) (Prod sf $ map prmPE ss)
           | Production nt (Prod sf ss) <- ps g ]
    , s0 = Prime (s0 g, 0)
    , _πs = _πs g
    , _μs = _μs g
    }

  prmPE :: ProdElem nt t -> ProdElem (Prime nt) t
  prmPE (NT nt) = NT $ Prime (nt, 0)
  prmPE (T x)   = T x
  prmPE Eps     = Eps
  
  lF :: Grammar s (Prime nt) t -> Grammar s (Prime nt) t
  lF g = let
    -- Longest common prefix of two lists
    lcp :: ProdElems (Prime nt) t -> ProdElems (Prime nt) t -> ProdElems (Prime nt) t
    lcp [] ys = []
    lcp xs [] = []
    lcp (x:xs) (y:ys)
      | x == y    = x : lcp xs ys
      | otherwise = []

    lcps :: [(Prime nt, ProdElems (Prime nt) t)]
    lcps = [ (nt0, maximumBy (comparing length)
                   [ lcp xs ys
                   | Production _ (Prod _ xs) <- filter ((== nt0) . getLHS) (ps g)
                   , Production _ (Prod _ ys) <- filter ((== nt0) . getLHS) (ps g)
                   , xs /= ys
                   ])
           | nt0 <- toList $ ns g ]

    --longest_lcps :: [(nt, ProdElems nt t)]
    --longest_lcps = filter (not . null . snd) lcps

    incr :: Prime nt -> Prime nt
    incr (Prime (nt, i)) = Prime (nt, i + 1)

    ps' :: [(Prime nt, ProdElems (Prime nt) t)] -> [Production s (Prime nt) t]
    ps' []           = ps g
    ps' ((nt, xs):_) =
        -- Unaffected productions
        [ Production nt0 (Prod v rhs)
        | Production nt0 (Prod v rhs) <- ps g
        , nt0 /= nt
        ]
      ++
        -- Unaffected productions
        [ Production nt0 (Prod v rhs)
        | Production nt0 (Prod v rhs) <- ps g
        , nt == nt0 && not (xs `isPrefixOf` rhs)
        ]
      ++
        -- Affected productions
        [ Production (incr nt0) (Prod v (drop (length xs) rhs))
        | Production nt0 (Prod v rhs) <- ps g
        , nt == nt0 && xs `isPrefixOf` rhs
        ]
      ++ [Production nt (Prod Pass $ xs ++ [NT $ incr nt])]
  {- [ (prime nt, drop (length xs) ys)
                    | (nt1, ys) <- ps g
                    , nt1 == nt
                    , xs `isPrefixOf` ys -}
                    
    in g { ps = ps' lcps }
  in lF . primeify

