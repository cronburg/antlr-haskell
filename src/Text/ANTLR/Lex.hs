{-# LANGUAGE FlexibleInstances, InstanceSigs, DeriveDataTypeable
    , ScopedTypeVariables #-}
{-|
  Module      : Text.ANTLR.Lex
  Description : Entrypoint for lexical and tokenization algorithms
  Copyright   : (c) Karl Cronburg, 2018
  License     : BSD3
  Maintainer  : karl@cs.tufts.edu
  Stability   : experimental
  Portability : POSIX

-}
module Text.ANTLR.Lex
  ( tokenize
  , Token(..)
  , tokenName, tokenValue
  ) where
import qualified Text.ANTLR.Grammar as G

import Text.ANTLR.LR (lr1Parse)
import Text.ANTLR.Parser

import qualified Data.Set.Monad as Set

import Data.Set.Monad
  ( fromList, member, (\\), empty, Set(..), toList
  , isSubsetOf, union, insert)
import Data.Data (Data(..), Typeable(..), toConstr, dataTypeConstrs, dataTypeOf)

import Text.ANTLR.Lex.Tokenizer

