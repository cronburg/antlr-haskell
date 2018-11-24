{-|
  Module      : Text.ANTLR.Language
  Description : Viewing a language as a set of words accepted
  Copyright   : (c) Karl Cronburg, 2018
  License     : BSD3
  Maintainer  : karl@cs.tufts.edu
  Stability   : experimental
  Portability : POSIX

-}
module Text.ANTLR.Language
  ( Alphabet(..), ascii, isASCII
  ) where
import Prelude hiding (Word)
import Data.Set.Monad (Set(..))
import qualified Data.Set.Monad as Set

import Data.Char

type Alphabet a = Set a

ascii :: Alphabet Char
ascii     = Set.fromList $ map chr [0 .. 127]

isASCII :: Char -> Bool
isASCII c = ord c < 127

type Word a = [a]

type Language a = Set (Word a) 

union :: (Ord a) => Set a -> Set a -> Set a
union  = Set.union

concat :: (Ord a) => Language a -> Language a -> Language a
concat a b = Set.fromList
  [ s ++ t
  | s <- Set.toList a
  , t <- Set.toList b
  ]

kleene = undefined

