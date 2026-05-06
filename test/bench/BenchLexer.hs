{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, QuasiQuotes
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, TemplateHaskell #-}
module BenchLexer where
import Language.ANTLR4

[g4|


grammar BenchLexer;

start : ID ID ID ;

ID : IdentifierHead IdentifierChar* ;
IdentifierHead : [a-zA-Z_] ;
IdentifierChar : [a-zA-Z0-9_] ;
Dec : [0-9]+ ;
Hex : '0x' [0-9a-fA-F]+ ;
Float : [0-9]+ '.' [0-9]+ ;
Str : '"' (~'"')* '"' ;
LCmt : '//' (~'\n')* '\n' ;
BCmt : '/*' (~'*')* '*/' ;
WS : [ \t\n\r]+ ;

|]
