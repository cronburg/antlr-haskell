{-# LANGUAGE ScopedTypeVariables #-}
module Text.ANTLR.Lex.Tokenizer where
import Text.ANTLR.Lex.Automata
import Text.ANTLR.Lex.DFA

import qualified Data.Set.Monad as Set
import Data.Set.Monad (member)

-- Token with name (n) and value (v)
data Token n v =
    Token n v
  | EOF
  | Error -- TODO
  deriving (Show)

instance Eq n => Eq (Token n v) where
  Token s _ == Token s1 _ = s == s1
  EOF       == EOF        = True
  Error     == Error      = True
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
tokenize ::
  forall s i n v. (Ord i, Eq i, Ord s, Eq s)
  => [DFA s i] -> (DFA s i -> n) -> (Lexeme s -> n -> v) -> [s] -> [Token n v]
tokenize dfas0 dfaName fncn input0 = let
    allTok :: [(DFA s i, State i)] -> [s] -> [Token n v]
    allTok dfaSims0 currInput = let
        oneTok :: [(DFA s i, State i)] -> [s] -> Maybe (Lexeme s, DFA s i)
        oneTok dfaSims []     = Nothing
        oneTok []      ss     = Nothing
        oneTok dfaSims (s:ss) = let
            dfaSims' =
              [ (dfa, stop)
              | (dfa, cursor)    <- dfaSims
              , (start, e, stop) <- Set.toList $ _Δ dfa
              , start == cursor && e == s ]

            accepting = [ dfa | (dfa, cursor) <- dfaSims', cursor `member` _F dfa ]

          in (case (oneTok dfaSims' ss, accepting) of
              (Nothing, [])   -> Nothing
              (Nothing, d:ds) -> Just ([s], d)
              (Just (l,d), _) -> Just (s:l, d))
      in case oneTok dfaSims0 currInput of
          Nothing     -> [Error]
          Just (l, d) ->
            Token (dfaName d) (fncn l (dfaName d))
            : allTok dfaSims0 (drop (length l) currInput)
  in allTok (zip dfas0 (map s0 dfas0)) input0

