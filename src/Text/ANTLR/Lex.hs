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

import Text.ANTLR.Lex.Tokenizer

