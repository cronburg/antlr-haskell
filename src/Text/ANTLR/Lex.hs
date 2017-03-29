{-# LANGUAGE FlexibleInstances, InstanceSigs, DeriveDataTypeable #-}
module Text.ANTLR.Lex where
import Text.ANTLR.Allstar.Grammar
import Text.ANTLR.AST (AST(..))

import Text.ANTLR.LR1 (lr1Parse)
import Text.ANTLR.Parser

import Data.Set (fromList, member, (\\), empty)
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

