{-# LANGUAGE FlexibleInstances, InstanceSigs, DeriveDataTypeable #-}
module Text.ANTLR.Allstar.Lex where
import Text.ANTLR.Allstar.Grammar
import Text.ANTLR.AST (AST(..))

import Text.ANTLR.LR1 (lr1Parse)
import Text.ANTLR.Parser

import Data.Set (fromList, member, (\\), empty)
import Data.Data (Data(..), Typeable(..), toConstr, dataTypeConstrs, dataTypeOf)

type Token t = (String, t)

tok2term :: Token t -> t
tok2term = snd

{-
regexGrammar = defaultGrammar
  { ns = fromList $ map show   [(minBound :: RegexNT) ..]
  , ts = fromList $ dataTypeConstrs $ dataTypeOf Regex
  , s0 = "Regex"
  , ps = [ ("Regex", Prod []) ]
  }

data RegexNT =
    Regex  | Union | Concat
  | Kleene | Pos   | Term
  deriving (Eq, Ord, Show, Enum, Bounded, Data, Typeable)

-- It's better if we use finite types for the terminals, that way we can more
-- easily generate Arbitrary instances, plus we get better type checking other
-- than "oh hey that's a String too, sure you can pass it in"
data RegexT =
    CharT Char
  | LeftParenT
  | RightParenT
  | UnionT
  | StarT
  deriving (Eq, Ord, Show, Data, Typeable)

instance Referent RegexNT where getSymbol = show . toConstr
instance Referent RegexT  where getSymbol = show . toConstr

regexParser :: [Icon RegexT] -> Maybe RegexAST
regexParser = lr1Parse regexGrammar act

type RegexAST = AST RegexNT RegexT

act :: ParseEvent RegexAST RegexNT String -> RegexAST
act (TermE (Icon c)) = Leaf (case c of
  "(" -> LeftParenT
  ")" -> RightParenT
  "|" -> UnionT
  "*" -> StarT
  _   -> CharT c)
act (TermE IconEps) = LeafEps
-}

