{-# LANGUAGE QuasiQuotes, TemplateHaskell, DeriveAnyClass, DeriveGeneric, TypeFamilies
		, DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, DeriveDataTypeable, FlexibleContexts #-}
module HelloParser where
import Language.ANTLR4
import Hello

$(g4_parsers helloAST helloGrammar)

