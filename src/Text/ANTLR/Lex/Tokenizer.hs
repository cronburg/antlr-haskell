{-# LANGUAGE ScopedTypeVariables, DeriveGeneric, DeriveAnyClass
  , OverloadedStrings #-}
module Text.ANTLR.Lex.Tokenizer where
import Text.ANTLR.Lex.Automata
import Text.ANTLR.Lex.DFA

import qualified Text.ANTLR.Set as Set
import Text.ANTLR.Set (Hashable, member, Generic(..), Set(..))

import Text.ANTLR.Pretty
import qualified Debug.Trace as D
import Data.List (find)
import qualified Data.Text as T

-- Token with name (n), value (v), and number of input symbols consumed to match
-- it.
data Token n v =
    Token n v Int
  | EOF
  | Error T.Text -- TODO
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

-- Token Names are Input Symbols to the parser
tokenName :: Token n v -> n
tokenName (Token n v _) = n

-- TODO: Debugging information goes in the value
tokenValue :: Token n v -> v
tokenValue (Token n v _) = v

tokenSize :: Token n v -> Int
tokenSize (Token _ _ i) = i
tokenSize EOF = 0

-- A Lexeme is a sequence of zero or more (matched) input symbols
type Lexeme s = [s]

{- *  dfaName: converts from DFAs to the names associated with them in
 -    the specification of the lexer.
 - *  fncn: function for constructing the value of a token from the lexeme
 -    matched (e.g. 'varName') and the associated token name (e.g. 'id')
 -}

type NDFA s i n = (n, DFA s i)

tokenize ::
  forall s i n v. (Eq i, Ord s, Eq s, Show s, Show i, Show n, Show v, Hashable i, Hashable s)
  => [(n, DFA s i)] -> (Lexeme s -> n -> v) -> [s] -> [Token n v]
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

-- Incremental tokenizer takes in the same list of DFAs and AST value
-- constructor function, but instead returns an incremental tokenizer function.
tokenizeInc
  :: forall s i n v. (Eq i, Ord s, Eq n, Eq s, Show s, Show i, Show n, Show v, Hashable i, Hashable s, Hashable n)
  => (n -> Bool) -> [(n, DFA s i)] -> (Lexeme s -> n -> v) -> (Set n -> [s] -> (Token n v, [s]))
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
                (t:_)  -> D.traceShowId t
      
      in (next, drop (sum $ map tokenSize $ next : ignored) input)
  in tI






