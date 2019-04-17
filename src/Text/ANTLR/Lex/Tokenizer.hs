{-# LANGUAGE ScopedTypeVariables, DeriveGeneric, DeriveAnyClass
  , OverloadedStrings #-}
{-|
  Module      : Text.ANTLR.Lex.Tokenizer
  Description : Tokenization algorithms
  Copyright   : (c) Karl Cronburg, 2018
  License     : BSD3
  Maintainer  : karl@cs.tufts.edu
  Stability   : experimental
  Portability : POSIX

-}
module Text.ANTLR.Lex.Tokenizer where
import Text.ANTLR.Lex.Automata
import Text.ANTLR.Lex.DFA

import qualified Text.ANTLR.Set as Set
import Text.ANTLR.Set (Hashable, member, Generic(..), Set(..))

import Text.ANTLR.Pretty
import qualified Debug.Trace as D
import Data.List (find)
import qualified Data.Text as T

-- | Token with names @n@, values @v@, and number of input symbols consumed to match
--   it.
data Token n v =
    Token n v Int  -- ^ Tokenized a token
  | EOF            -- ^ The end-of-file token
  | Error T.Text   -- ^ Error encountered while tokenizing
  deriving (Show, Ord, Generic, Hashable)

instance (Prettify n, Prettify v) => Prettify (Token n v) where
  prettify EOF = pStr "EOF"
  prettify (Error s) = pStr "Token Error: " >> pStr s
  prettify (Token n v i) =
    prettify v

instance Eq n => Eq (Token n v) where
  Token s _ _ == Token s1 _ _ = s == s1
  EOF         == EOF          = True
  Error s     == Error s1     = s == s1
  _           == _            = False

-- | Token Names are Input Symbols to the parser.
tokenName :: Token n v -> n
tokenName (Token n v _) = n

-- | Get the value of a token, ignoring its name.
tokenValue :: Token n v -> v
tokenValue (Token n v _) = v

-- | Get the number of characters from the input that this token matched on.
tokenSize :: Token n v -> Int
tokenSize (Token _ _ i) = i
tokenSize EOF = 0

-- | A Lexeme is a sequence of zero or more (matched) input symbols
type Lexeme s = [s]

-- | A named DFA over symbols @s@, indices @i@, and names @n@.
type NDFA s i n = (n, DFA s i)

-- | Entrypoint for tokenizing an input stream given a list of named DFAs that
--   we can match on.
--   
--   > @dfaTuples@: converts from DFAs to the names associated with them in
--     the specification of the lexer.
--
--   > @fncn@: function for constructing the value of a token from the lexeme
--     matched (e.g. @varName@) and the associated token name (e.g. @id@)
--
tokenize ::
  forall s i n v. (Eq i, Ord s, Eq s, Show s, Show i, Show n, Show v, Hashable i, Hashable s)
  => [(n, DFA s i)]       -- ^ Association list of named DFAs.
  -> (Lexeme s -> n -> v) -- ^ Constructs the value of a token from lexeme matched.
  -> [s]                  -- ^ The input string.
  -> [Token n v]          -- ^ The tokenized tokens.
tokenize dfaTuples fncn input0 = let

    dfas0 = map snd dfaTuples

    allTok :: [(NDFA s i n, State i)] -> [s] -> [Token n v]
    allTok dfaSims0 currInput = let
        oneTok :: [(NDFA s i n, State i)] -> [s] -> Maybe (Lexeme s, NDFA s i n)
        oneTok dfaSims []     = Nothing
        oneTok []      ss     = Nothing
        oneTok dfaSims (s:ss) = let
            dfaSims' =
              [ ((n, dfa), stop)
              | ((n, dfa), cursor)     <- dfaSims
              , (start, es, stop) <- Set.toList $ _Δ dfa
              , start == cursor && s `edgeMember` es ]

            accepting = [ (n,dfa) | ((n, dfa), cursor) <- dfaSims', cursor `member` _F dfa ]

          in (case (oneTok dfaSims' ss, accepting) of
              (Nothing, [])   -> Nothing
              (Nothing, d:ds) -> Just ([s], d)
              (Just (l,d), _) -> Just (s:l, d))
      in case (currInput, oneTok dfaSims0 currInput) of
          ([], _)       -> [EOF]
          (ss, Nothing) -> [Error $ T.pack $ show ss]
          (ss, Just (l, (name,d))) ->
            Token name (fncn l name) (length l)
            : allTok dfaSims0 (drop (length l) currInput)
  in allTok (zip dfaTuples (map s0 dfas0)) input0

-- | Incremental tokenizer takes in the same list of DFAs and AST value
--   constructor function, but instead returns an incremental tokenizer function
--   that expects a set of names that we currently expect to tokenize on,
--   the current input stream, and returns a single tokenized token along
--   with the modified input stream to iteratively call 'tokenizeInc' on.
tokenizeInc
  :: forall s i n v. (Eq i, Ord s, Eq n, Eq s, Show s, Show i, Show n, Show v, Hashable i, Hashable s, Hashable n)
  => (n -> Bool)                         -- ^ Function that returns True on DFA names we wish to filter __out__ of the results.
  -> [(n, DFA s i)]                      -- ^ Closure over association list of named DFAs.
  -> (Lexeme s -> n -> v)                -- ^ Token value constructor from lexemes.
  -> (Set n -> [s] -> (Token n v, [s]))  -- ^ The incremental tokenizer closure.
tokenizeInc filterF dfaTuples fncn = let

    tI :: Set n -> [s] -> (Token n v, [s])
    tI ns input = let
        
        dfaTuples'  = filter (\(n,_) -> n `Set.member` ns || filterF n) dfaTuples
        tokenized   = tokenize dfaTuples' fncn input
        
        filterF' (Token n _ _) = filterF n
        filterF' _             = False
        
        ignored     = takeWhile filterF' tokenized
        nextTokens  = dropWhile filterF' tokenized
        -- Yayy lazy function evaluation.
        next = case nextTokens of
                []     -> EOF
                (t:_)  -> t --D.traceShowId t
      
      in (next, drop (sum $ map tokenSize $ next : ignored) input)
  in tI

tokenizeIncAll
  :: forall s i n v. (Eq i, Ord s, Eq n, Eq s, Show s, Show i, Show n, Show v, Hashable i, Hashable s, Hashable n)
  => (n -> Bool)                         -- ^ Function that returns True on DFA names we wish to filter __out__ of the results.
  -> [(n, DFA s i)]
  -> (Lexeme s -> n -> v)
  -> (Set n -> [s] -> [(Token n v, [s])])
tokenizeIncAll filterF dfaTuples fncn = let
    
    tI :: Set n -> [s] -> [(Token n v, [s])]
    tI ns input = let
        
        dfaTuples'  = filter (\(n,_) -> n `Set.member` ns || filterF n) dfaTuples
        tokenized   = tokenize dfaTuples' fncn input
        
        filterF' (Token n _ _) = filterF n
        filterF' _             = False
        
        ignored     = takeWhile filterF' tokenized
        nextTokens  = dropWhile filterF' tokenized
        -- Yayy lazy function evaluation.
        next = case nextTokens of
                []     -> EOF
                (t:_)  -> t --D.traceShowId t
      
        input' = drop (sum $ map tokenSize $ next : ignored) input
      
        ns' = case next of
                Token n _ _ -> n `Set.delete` ns
                _ -> Set.empty

      in (next, input') : tI ns' input'
  in tI

