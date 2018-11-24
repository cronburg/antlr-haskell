{-|
  Module      : Text.ANTLR.Common
  Description : Haskell-level helper functions used throughout Text.ANTLR
  Copyright   : (c) Karl Cronburg, 2018
  License     : BSD3
  Maintainer  : karl@cs.tufts.edu
  Stability   : experimental
  Portability : POSIX

-}
module Text.ANTLR.Common where

concatWith cs [] = []
concatWith cs [x] = x
concatWith cs (x:xs) = x ++ cs ++ concatWith cs xs

