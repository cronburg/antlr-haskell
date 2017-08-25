module Text.ANTLR.Allstar
  ( parse
  , Token(..)
  , GrammarSymbol(..)
  , ATNEnv, ATN
  ) where

import ParserGenerator.AllStar

import qualified Text.ANTLR.Parser as P

-- | Go from an Allstar AST to the AST type used internally in this package
fromAllstarAST :: AST nts t -> P.AST nts t
fromAllstarAST (Node nt asts) = P.AST nt [] (map fromAllstarAST asts)
fromAllstarAST (Leaf tok)     = P.Leaf tok



