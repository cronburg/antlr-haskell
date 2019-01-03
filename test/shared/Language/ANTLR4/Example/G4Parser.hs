{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, QuasiQuotes
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, TemplateHaskell, FlexibleContexts #-}
module Language.ANTLR4.Example.G4Parser where
import Language.ANTLR4
import Language.ANTLR4.Example.G4

$(g4_parsers g4BasicAST g4BasicGrammar)

