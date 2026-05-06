{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, QuasiQuotes
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, TemplateHaskell #-}
module BenchLits where
import Language.ANTLR4

[g4|

grammar BenchLits;

start : r0 ;

r0 : r1 | r2 | A | ';' | '.' ;
r1 : r2 | r3 | A | '.' | ':' ;
r2 : r3 | r4 | A | ':' | '->' ;
r3 : r4 | r5 | A | '->' | '?' ;
r4 : r5 | r6 | A | '?' | '!' ;
r5 : r6 | r7 | A | '!' | '+' ;
r6 : r7 | r8 | A | '+' | '-' ;
r7 : r8 | r9 | A | '-' | '*' ;
r8 : r9 | r10 | A | '*' | '/' ;
r9 : r10 | r11 | A | '/' | '(' ;
r10 : r11 | r12 | A | '(' | ')' ;
r11 : r12 | r13 | A | ')' | '[' ;
r12 : r13 | r14 | A | '[' | ']' ;
r13 : r14 | r15 | A | ']' | '{' ;
r14 : r15 | r16 | A | '{' | '}' ;
r15 : r16 | r17 | A | '}' | '=' ;
r16 : r17 | r18 | A | '=' | '==' ;
r17 : r18 | r19 | A | '==' | '!=' ;
r18 : r19 | r20 | A | '!=' | '<' ;
r19 : r20 | r21 | A | '<' | '>' ;
r20 : r21 | r22 | A | '>' | '<=' ;
r21 : r22 | r23 | A | '<=' | '>=' ;
r22 : r23 | r24 | A | '>=' | '&&' ;
r23 : r24 | r25 | A | '&&' | '||' ;
r24 : r25 | r26 | A | '||' | 'let' ;
r25 : r26 | r27 | A | 'let' | 'var' ;
r26 : r27 | r28 | A | 'var' | 'if' ;
r27 : r28 | r29 | A | 'if' | 'else' ;
r28 : r29 | r0 | A | 'else' | 'return' ;
r29 : r0 | r1 | A | 'return' | ';' ;

A : 'a' ;
WS : [ \t\n\r]+ ;
|]
