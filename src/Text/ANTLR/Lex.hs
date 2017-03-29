{-# LANGUAGE FlexibleInstances, InstanceSigs, DeriveDataTypeable
    , ScopedTypeVariables #-}
module Text.ANTLR.Lex where
import qualified Text.ANTLR.Allstar.Grammar as G
import Text.ANTLR.AST (AST(..))

import Text.ANTLR.LR1 (lr1Parse)
import Text.ANTLR.Parser

import Data.Set (fromList, member, (\\), empty, Set(..), toList)
import Data.Data (Data(..), Typeable(..), toConstr, dataTypeConstrs, dataTypeOf)

-- Token with name (n) and value (v).
newtype Token n v = Token (n, v)

instance Eq n => Eq (Token n v) where
  Token (s,_) == Token (s1,_) = s == s1

-- Token Names are Input Symbols to the parser
tokenName :: Token n v -> n
tokenName (Token x) = fst x

-- TODO: Debugging information goes in the value
tokenValue :: Token n v -> v
tokenValue (Token x) = snd x

data Regex s =
    Epsilon
  | Symbol   s
  | Union     (Regex s) (Regex s)
  | Concat    (Regex s) (Regex s)
  | Kleene    (Regex s)
  | PosClos   (Regex s)
  | Question  (Regex s)
  | CharClass [s] -- TODO: Set s, and ranges of characters
-- TODO: Lex regexs (e.g. complement sets, escape chars, ...)

data NFA s i = NFA
  { _S :: Set (State i)        -- Finite set of states.
  , _Σ :: Set s                -- Input (NFA edge) alphabet
  , _Δ :: Set (Transition s i) -- Transition function
  , s0 :: State i              -- Start state
  , _F :: Set (State i)        -- Accepting states
  }

type Transition s i = (State i, Edge s, Set (State i))

type State i = i
type Edge  s = s

data Result = Accept | Reject

validStartState nfa = s0 nfa `member` _S nfa

validFinalStates nfa = and [s `member` _S nfa | s <- toList $ _F nfa]

validTransitions :: forall s i. (Ord (State i)) => NFA s i -> Bool
validTransitions nfa = let
    vT :: [Transition s i] -> Bool
    vT [] = True
    vT ((s1, e, s2):rest) =
         s1 `member` _S nfa
      && (and [s2' `member` _S nfa | s2' <- toList s2])
      && vT rest
  in vT $ (toList . _Δ) nfa

{-
regexGrammar = defaultGrammar
  { ns = fromList $ dataTypeConstrs $ dataTypeOf Regex
  , ts = fromList $ dataTypeConstrs $ dataTypeOf CharT
  , s0 = Regex
  , ps = [ (Regex, Prod []) ]
  }

data RegexNT =
    Regex  | Union | Concat
  | Kleene | Pos   | Term
  deriving (Eq, Ord, Show, Enum, Bounded, Data, Typeable)

-- Regex Tokens:
data RegexT =
    CharT Char
  | LeftParenT
  | RightParenT
  | UnionT
  | StarT
  deriving (Eq, Ord, Show, Enum, Bounded, Data, Typeable)

-- Regex Terminals:
data RegexTerm = 
    CharTerm Char
  | LeftParenTerm
  | RightParenTerm
  | UnionTerm
  | StarTerm

--dataTypeConstrs $ dataTypeOf Regex

instance Referent RegexNT where getSymbol = show . toConstr
instance Referent RegexT  where getSymbol = show . toConstr

regexParser :: [Icon RegexT] -> Maybe RegexAST
regexParser = lr1Parse regexGrammar act

type RegexAST = AST RegexNT RegexTerm

act :: ParseEvent RegexAST RegexNT RegexT -> RegexAST
act (TermE (Icon c)) = Leaf (case c of
  LeftParenT -> LeftParenTerm
  RightParenT -> RightParenTerm
  UnionT -> UnionTerm
  StarT -> StarTerm
  _   -> CharTerm c)
act (TermE IconEps) = LeafEps

-}

