{-# LANGUAGE QuasiQuotes, DeriveAnyClass, DeriveGeneric, TypeFamilies
		, DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances #-}
module Test.Language.ANTLR4.Hello where
import Language.ANTLR4

[g4|
	// Hello World grammar
	// https://github.com/antlr/grammars-v4/blob/master/antlr4/examples/Hello.g4
	grammar Hello;
	r   : 'hello' WS ID;
	ID  : [a-zA-Z]+ -> String;
	WS  : [ \t\r\n]+ -> String;
|]

