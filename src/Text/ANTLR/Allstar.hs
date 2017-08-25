module Text.ANTLR.Allstar
  ( parse
  , Token(..)
  , GrammarSymbol(..)
  , ATNEnv, ATN
  ) where

import ParserGenerator.AllStar

import qualified Text.ANTLR.Parser as P
import qualified Text.ANTLR.Grammar as G

-- | Go from an Allstar AST to the AST type used internally in this package
fromAllstarAST :: AST nts t -> P.AST nts t
fromAllstarAST (Node nt asts) = P.AST nt [] (map fromAllstarAST asts)
fromAllstarAST (Leaf tok)     = P.Leaf tok

-- | Go from an antlr-haskell Grammar to an Allstar ATNEnv
--   TODO: Handle predicate and mutator state during the conversion
atnOf :: G.Grammar s nt t -> ATN nt t
atnOf = undefined

