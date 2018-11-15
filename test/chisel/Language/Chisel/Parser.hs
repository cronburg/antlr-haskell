{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, QuasiQuotes
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, TemplateHaskell #-}
module Language.Chisel.Parser
  ( module Language.Chisel.Grammar
  , glrParseFast
  ) where
import Language.ANTLR4
import Language.Chisel.Syntax as S
import Language.Chisel.Grammar

import qualified GHC.Types as G
import qualified Text.ANTLR.LR as LR

$(mkLRParser the_ast chiselGrammar)

