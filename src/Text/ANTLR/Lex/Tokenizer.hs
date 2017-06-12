{-# LANGUAGE ScopedTypeVariables, DeriveGeneric, DeriveAnyClass
  , OverloadedStrings #-}
module Text.ANTLR.Lex.Tokenizer where
import Text.ANTLR.Lex.Automata
import Text.ANTLR.Lex.DFA

import qualified Text.ANTLR.Set as Set
import Text.ANTLR.Set (Hashable, member, Generic(..))

import Text.ANTLR.Pretty
import qualified Debug.Trace as D
import Data.List (find)
import qualified Data.Text as T

-- Token with name (n) and value (v)
data Token n v =
    Token n v
  | EOF
  | Error T.Text -- TODO
  deriving (Show, Ord, Generic, Hashable)

instance (Prettify n, Prettify v) => Prettify (Token n v) where
  prettify EOF = pStr "EOF"
  prettify (Error s) = pStr "Token Error: " >> pStr s
  prettify (Token n v) = do
    prettify n
    pParens $ prettify v

instance Eq n => Eq (Token n v) where
  Token s _ == Token s1 _ = s == s1
  EOF       == EOF        = True
  Error s   == Error s1   = s == s1
  _         == _          = False

-- Token Names are Input Symbols to the parser
tokenName :: Token n v -> n
tokenName (Token n v) = n

-- TODO: Debugging information goes in the value
tokenValue :: Token n v -> v
tokenValue (Token n v) = v

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
            Token name (fncn l name)
            : allTok dfaSims0 (drop (length l) currInput)
  in allTok (zip dfaTuples (map s0 dfas0)) input0

