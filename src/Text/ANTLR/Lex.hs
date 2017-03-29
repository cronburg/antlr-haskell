{-# LANGUAGE FlexibleInstances, InstanceSigs, DeriveDataTypeable
    , ScopedTypeVariables #-}
module Text.ANTLR.Lex where
import qualified Text.ANTLR.Allstar.Grammar as G
import Text.ANTLR.AST (AST(..))

import Text.ANTLR.LR1 (lr1Parse)
import Text.ANTLR.Parser

import qualified Data.Set as Set

import Data.Set
  ( fromList, member, (\\), empty, Set(..), toList
  , isSubsetOf, union, insert)
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

