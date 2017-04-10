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

