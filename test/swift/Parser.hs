{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, QuasiQuotes
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, TemplateHaskell #-}
module Parser
  ( module Grammar
  , glrParse, ast2sexpr
  ) where
import Language.ANTLR4
import Grammar

import qualified Text.ANTLR.LR as LR

$(g4_parsers swiftAST swiftGrammar)
-- $(mkLRParser the_ast sexpressionGrammar)

