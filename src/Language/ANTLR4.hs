{-|
  Module      : Language.ANTLR4
  Description : Primary entrypoint for top-level antlr-haskell users
  Copyright   : (c) Karl Cronburg, 2018
  License     : BSD3
  Maintainer  : karl@cs.tufts.edu
  Stability   : experimental
  Portability : POSIX
-}
module Language.ANTLR4 (
  -- * Functions
  -- | Compile-time support for expanding LR-specific data types:
    mkLRParser
  -- | Other basic functions used in generated code:
  , (&&&)
  -- * Module exports
  -- | Most importantly for the Grammar type so that the quasiquoter can generate
  -- new grammar itself:
  , module Text.ANTLR.Grammar
  -- | Supporting data types and instances so that the spliced AST translator
  -- functions can talk about parse events, tokens, and EOF:
  , module Text.ANTLR.Parser
  -- | Regular expressions used during tokenization, as opposed to
  -- 'Language.ANTLR4.Regex' which are regexes used for G4 parsing:
  , module Text.ANTLR.Lex.Regex
  -- | The G4 quasiquoter and accompanying grammar:
  , g4, g4_parsers
  -- | For defining pretty-printable instances of quasiquoter-generated data types:
  , module Text.ANTLR.Pretty
  -- | Tokenizer:
  , module T
  -- * Type exports
  -- | Typeclass instances for quasiquoter-generated data types:
  , Hashable(..), Generic(..), Data(..), Lift(..)
  -- | Parser interface data types:
  , S.Set(..), T.Token(..), LRResult(..)
  -- | Grammar interface data types:
  , Directive(..), PRHS(..), TermAnnot(..)
  )
where

import Text.ANTLR.Grammar
import Text.ANTLR.Parser

import Text.ANTLR.LR as LR
import Text.ANTLR.Lex.Tokenizer as T
import Text.ANTLR.Set as S

import Text.ANTLR.Set (Hashable(..), Generic(..))
import Text.ANTLR.Pretty
import Control.Arrow ( (&&&) )
import Text.ANTLR.Lex.Regex

import Language.ANTLR4.G4
import Language.ANTLR4.Parser
import Language.ANTLR4.Boot.Quote (mkLRParser, g4_parsers)

import Data.Data (Data(..))
import Language.Haskell.TH.Lift (Lift(..))

import Language.ANTLR4.Boot.Syntax
  (Directive(..), PRHS(..), TermAnnot(..), ProdElem(..))

