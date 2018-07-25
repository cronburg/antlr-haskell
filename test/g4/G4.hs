{-# LANGUAGE TemplateHaskell #-}
module G4 where

import System.IO.Unsafe (unsafePerformIO)
import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck (Property, quickCheck, (==>))
import qualified Test.QuickCheck.Monadic as TQM

import Language.ANTLR4 hiding (tokenize, Regex(..))
import Text.ANTLR.Grammar
import qualified Language.ANTLR4.Example.Optionals as Opt
import Language.ANTLR4.Example.G4 as G4
--import Language.ANTLR4.Example.G4 (g4BasicGrammar, G4BasicNTSymbol, G4BasicTSymbol, G4BasicAST)
import Text.ANTLR.Parser (AST(..))
import qualified Text.ANTLR.LR as LR
import qualified Text.ANTLR.Lex.Tokenizer as T

import qualified Language.ANTLR4.G4 as P -- Parser

import Data.Map.Internal (fromList)
import Text.ANTLR.MultiMap (Map(..))
import qualified Text.ANTLR.Set as S
import qualified Data.HashSet as H

$(mkLRParser G4.the_ast g4BasicGrammar)

