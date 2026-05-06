{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, QuasiQuotes
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, TemplateHaskell #-}
module BenchBase where
import Language.ANTLR4

[g4|

grammar BenchBase;

start : r0 ;

r0 : r1 | r2 | A | B ;
r1 : r2 | r3 | A | B ;
r2 : r3 | r4 | A | B ;
r3 : r4 | r5 | A | B ;
r4 : r5 | r6 | A | B ;
r5 : r6 | r7 | A | B ;
r6 : r7 | r8 | A | B ;
r7 : r8 | r9 | A | B ;
r8 : r9 | r10 | A | B ;
r9 : r10 | r11 | A | B ;
r10 : r11 | r12 | A | B ;
r11 : r12 | r13 | A | B ;
r12 : r13 | r14 | A | B ;
r13 : r14 | r15 | A | B ;
r14 : r15 | r16 | A | B ;
r15 : r16 | r17 | A | B ;
r16 : r17 | r18 | A | B ;
r17 : r18 | r19 | A | B ;
r18 : r19 | r20 | A | B ;
r19 : r20 | r21 | A | B ;
r20 : r21 | r22 | A | B ;
r21 : r22 | r23 | A | B ;
r22 : r23 | r24 | A | B ;
r23 : r24 | r25 | A | B ;
r24 : r25 | r26 | A | B ;
r25 : r26 | r27 | A | B ;
r26 : r27 | r28 | A | B ;
r27 : r28 | r29 | A | B ;
r28 : r29 | r0 | A | B ;
r29 : r0 | r1 | A | B ;

A : 'a' ;
B : 'b' ;
WS : [ \t\n\r]+ ;
|]
