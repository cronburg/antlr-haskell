{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, QuasiQuotes
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, TemplateHaskell, FlexibleContexts #-}
module Language.ANTLR4.Example.OptionalParser where
import Language.ANTLR4
import Language.ANTLR4.Example.Optional

$(g4_parsers optionalAST optionalGrammar)

