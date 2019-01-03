{-# LANGUAGE QuasiQuotes, TemplateHaskell, DeriveAnyClass, DeriveGeneric, TypeFamilies
		, DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, DeriveDataTypeable, FlexibleContexts #-}
module Language.ANTLR4.Example.HelloParser where
import Language.ANTLR4
import Language.ANTLR4.Example.Hello

$(g4_parsers helloAST helloGrammar)

