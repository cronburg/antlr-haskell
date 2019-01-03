{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, QuasiQuotes
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, TemplateHaskell #-}
module Parser
  ( module Grammar
  , glrParse, ast2sexpr
--  , glrParseFast
  ) where
import Language.ANTLR4
import Grammar

--import qualified GHC.Types as G
import qualified Text.ANTLR.LR as LR

$(g4_parsers sexpressionAST sexpressionGrammar)

-- $(mkLRParser the_ast sexpressionGrammar)

