{-# LANGUAGE TemplateHaskell #-}
module G4Fast where
import Language.ANTLR4
import G4
import G4Parser

$(mkLRParser g4BasicAST g4BasicGrammar)

{-
import System.IO.Unsafe (unsafePerformIO)
import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck (Property, quickCheck, (==>))
import qualified Test.QuickCheck.Monadic as TQM
-}

{-
import Language.ANTLR4 hiding (tokenize)
import Text.ANTLR.Grammar
import qualified Language.ANTLR4.Example.Optionals as Opt
import Language.ANTLR4.Example.G4 as G4
--import Language.ANTLR4.Example.G4 (g4BasicGrammar, G4BasicNTSymbol, G4BasicTSymbol, G4BasicAST)
import Text.ANTLR.Parser (AST(..))
import qualified Text.ANTLR.LR as LR
import qualified Text.ANTLR.Lex.Tokenizer as T
-}

{-
import Data.Map.Internal (fromList)
import qualified Text.ANTLR.MultiMap as M
import Text.ANTLR.MultiMap (Map(..))
import qualified Text.ANTLR.Set as S
import qualified Data.HashSet as H
-}

