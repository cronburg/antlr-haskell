{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, QuasiQuotes
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, TemplateHaskell #-}
module Grammar where
import Language.ANTLR4

[g4|

grammar Bench10;

start : r0 ;

r0 : r1 | r2 | A | B ;
r1 : r2 | r3 | A | B ;
r2 : r3 | r4 | A | B ;
r3 : r4 | r5 | A | B ;
r4 : r5 | r6 | A | B ;
r5 : r6 | r7 | A | B ;
r6 : r7 | r8 | A | B ;
r7 : r8 | r9 | A | B ;
r8 : r9 | r0 | A | B ;
r9 : r0 | r1 | A | B ;

A : 'a' ;
B : 'b' ;
WS : [ \t\n\r]+ ;

|]
