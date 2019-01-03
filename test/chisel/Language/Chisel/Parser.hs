{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, QuasiQuotes
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, TemplateHaskell #-}
module Language.Chisel.Parser
  ( module Language.Chisel.Grammar
  , Language.Chisel.Parser.tokenize, glrParseFast
  , parse, glrParse
  ) where
import Language.ANTLR4
import Language.Chisel.Syntax as S
import Language.Chisel.Grammar

$(g4_parsers chiselAST chiselGrammar)

$(mkLRParser chiselAST chiselGrammar)

parse = glrParse isWhitespace


