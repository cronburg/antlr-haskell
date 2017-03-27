{-# LANGUAGE ScopedTypeVariables #-}
module Text.ANTLR.LL1
  ( recognize
  , first, follow
  , Token(..)
  , foldWhileEpsilon
  , isLL1, parseTable
  , predictiveParse
  , ParseEvent(..)
  , isToken, isEps', isEOF
  ) where
import Text.ANTLR.Allstar.Grammar
import Text.ANTLR.Allstar.ATN
import Data.Set ( Set(..), singleton, fromList, union, empty, member, size, toList
                , insert, delete, intersection, elemAt
                )

import qualified Data.Map.Strict as M

import System.IO.Unsafe (unsafePerformIO)
uPIO = unsafePerformIO

-- An LL1 Token (as used in first and follow sets) is either a
-- terminal in the grammar's alphabet, or an epsilon
data Token t =
    Token t
  | Eps'
  | EOF -- End of input really, but EOF is ubiquitous.
  deriving (Eq, Ord, Show)

isToken Token{} = True
isToken _ = False

isEps' Eps' = True
isEps' _    = False

isEOF EOF = True
isEOF _   = False

-- Fold while the given pred function is true:
foldWhile :: (a -> b -> Bool) -> (a -> b -> b) -> b -> [a] -> b
foldWhile pred fncn = let
    fW' b0 []     = b0
    fW' b0 [a]    = b0
    fW' b0 (a:as)
      | pred a b0 = fW' (fncn a b0) as
      | otherwise = b0
  in fW'

epsIn set _ = Eps' `member` set

-- Fold over a set of Symbols while all the previous sets of
-- symbols contains an epsilon.
foldWhileEpsilon fncn b0 []     = empty
foldWhileEpsilon fncn b0 [a]    = fncn a b0
foldWhileEpsilon fncn b0 (a:as)
  | epsIn a b0 = foldWhile epsIn fncn (fncn a b0) as
  | otherwise  = fncn a b0

first ::
  forall t nt. (NonTerminal nt, Terminal t, Ord nt, Ord t)
  => Grammar () nt t -> [ProdElem nt t] -> Set (Token t)
first g = let
    firstOne :: Set (ProdElem nt t) -> ProdElem nt t -> Set (Token t)
    firstOne _ t@(T x) = singleton $ Token x
    firstOne _ Eps     = singleton Eps'
    firstOne busy nt@(NT x)
      | nt `member` busy = empty
      | otherwise = foldr union empty
            [ foldWhileEpsilon union empty
              [ firstOne (insert nt busy) y
              | y <- (\(Prod ss) -> ss) rhs
              ]
            | (_, rhs) <- prodsFor g x
            , isProd rhs
            ]
    
    firstMany :: [Set (Token t)] -> Set (Token t)
    firstMany []   = singleton Eps'
    firstMany (ts:tss)
      | Eps' `member` ts = ts `union` firstMany tss
      | otherwise        = ts
  in firstMany . map (firstOne empty)

follow ::
  forall nt t. (NonTerminal nt, Terminal t, Ord nt, Ord t)
  => Grammar () nt t -> nt -> Set (Token t)
follow g = let
    follow' busy _B
      | _B `member` busy = empty
      | otherwise = let

        busy' = insert _B busy
        
        followProd :: nt -> Symbols nt t -> Set (Token t)
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
              (delete Eps' $ first g β)
              `union`
              -- If A -> αBβ and Epsilon `member` FIRST(β), then everything
              -- in FOLLOW(A) is in FOLLOW(B)
              (if Eps' `member` first g β
                then follow' busy' _A
                else empty
              )

            -- Start state contains EOF (aka '$', end of input) in FOLLOW()
      in  (if _B == s0 g then singleton EOF else empty)
          `union`
          foldr union empty
            [ followProd lhs_nt ss
            | (lhs_nt, Prod ss) <- filter (isProd . snd) . ps $ g
            ]
  in follow' empty

-- A -> α | β for all distinct ordered pairs of α and β,
--      first(α) `intersection` first(β) == empty
-- and if epsilon is in α, then
--      first(α) `intersection` follow(A) == empty
isLL1 :: (NonTerminal nt, Terminal t, Eq nt, Ord nt, Eq t, Ord t) => Grammar () nt t -> Bool
isLL1 g =
  validGrammar g && and
      [  (first g α `intersection` first  g β  == empty)
      && (not (Eps' `member` first g α)
         || ((first g α `intersection` follow g nt) == empty))
      | nt       <- toList $ ns g
      , (Prod α) <- map snd $ prodsFor g nt
      , (Prod β) <- map snd $ prodsFor g nt
      , α /= β
      ]

type Key nt t = (nt, Token t)

-- All possible productions we could reduce. Empty implies parse error,
-- singleton implies unambiguous entry, multiple implies ambiguous:
type Value nt t = Set (Symbols nt t)

ambigVal :: Value nt t -> Bool
ambigVal = (1 >) . size

-- M[A,s] = α for each symbol s `member` FIRST(α)
type ParseTable nt t = M.Map (Key nt t) (Value nt t)

parseTable' ::
  forall nt. forall t. (NonTerminal nt, Terminal t, Ord nt, Ord t, Eq t, Eq nt)
  => (Value nt t -> Value nt t -> Value nt t) -> Grammar () nt t-> ParseTable nt t
parseTable' fncn g = let

    insertMe ::
      (NonTerminal nt, Terminal t)
      => (nt, Token t, Symbols nt t) -> (ParseTable nt t -> ParseTable nt t)
    insertMe (_A, a, α) = M.insertWith fncn (_A, a) $ singleton α

  in
    foldr insertMe M.empty
      -- For each terminal a `member` FIRST(α), add A -> α to M[A,α]
      [ (_A, Token a, α)
      | (_A, Prod α) <- ps g
      , Token a <- toList $ first g α
      ]
    `M.union`
    foldr insertMe M.empty
      -- If Eps `member` FIRST(α), add A -> α to M[A,b]
      -- for each b `member` FOLLOW(A)
      [ (_A, Token b, α)
      | (_A, Prod α) <- ps g
      , Eps' `member` first g α
      , Token b <- toList $ follow g _A
      ]
    `M.union`
    foldr insertMe M.empty
      -- If Eps `member` FIRST(α)
      -- , AND EOF `member` FOLLOW(_A)
      -- add A -> α to M[A,EOF]
      [ (_A, EOF, α)
      | (_A, Prod α) <- ps g
      , Eps' `member` first g α
      , EOF  `member` follow g _A
      ]

parseTable :: 
  forall nt. forall t. (NonTerminal nt, Terminal t, Ord nt, Ord t, Eq t, Eq nt)
  => Grammar () nt t -> ParseTable nt t
parseTable = parseTable' union

-- Action function is given the nonterminal we just matched on, and the
-- corresponding list of symbols in the RHS of the matched production
-- alternative, and the result of recursively 
data ParseEvent ast nt t =
    TermE t
  | NonTE (nt, Symbols nt t, [ast])
  | EpsE
type Action ast nt t = ParseEvent ast nt t -> ast

data TreeNode ast nt t =
    Comp   ast
  | InComp nt (Symbols nt t) [ast] Int
  deriving (Eq, Ord, Show)

type StackTree ast nt t = [TreeNode ast nt t]

isComp (Comp _) = True
isComp _ = False
isInComp = not . isComp

recognize ::
  (NonTerminal nt, Terminal t, Ord nt, Ord t, Show nt, Show t)
  => Grammar () nt t -> [Token t] -> Bool
recognize g = (Nothing /=) . predictiveParse g (const ())

predictiveParse ::
  forall nt t ast. (Show nt, Show t, Show ast, NonTerminal nt, Terminal t, Ord nt, Ord t)
  => Grammar () nt t -> Action ast nt t -> [Token t] ->  Maybe ast
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
    --pushStack :: ProdElem -> Symbols -> StackTree ast -> StackTree ast
    pushStack (NT nt) ss stree = reduce $ InComp nt ss [] (length ss) : stree
    pushStack (T t)   _  (InComp nt ss asts i:stree) = reduce $ InComp nt ss (act (TermE t) : asts) (i - 1) : stree
    pushStack Eps     _  (InComp nt ss asts i:stree) = reduce $ InComp nt ss (act EpsE : asts) (i - 1) : stree
    
    _M :: ParseTable nt t
    _M = parseTable g

    -- input word LL1 symbols -> Stack of symbols -> AST
    -- [ast] - a stack (list) of the asts the user has computed for us
    --         intermixed (in proper order) with the Terminals in the production
    --         rule for which we reduced the NonTerminal in question.
    parse' :: [Token t] -> Symbols nt t -> StackTree ast nt t -> Maybe (StackTree ast nt t) --Maybe ast
    parse' [EOF] [] asts  = Just asts  -- Success!
    parse' _     [] asts  = Nothing    -- Parse failure because no end of input found
    parse' (Token a:ws) (T x:xs) asts
      | x == a    = parse' ws xs $ pushStack (T x) [] asts
      | otherwise = Nothing
    parse' ws@(a:_) (NT _X:xs) asts =
        case (_X, a) `M.lookup` _M of
          Nothing -> Nothing
          Just ss -> case (size ss, 0 `elemAt` ss) of
              (1,ss') -> parse' ws (ss' ++ xs) (pushStack (NT _X) ss' asts)
              _       -> Nothing
    parse' ws (Eps:xs) asts = parse' ws xs (pushStack Eps [] asts)
    parse' ws xs asts =
      uPIO (print (ws,xs,asts)) `seq` undefined

  in do asts <- parse' w0 [NT $ s0 g] []
        case asts of
          [Comp ast] -> Just ast
          _          -> Nothing

