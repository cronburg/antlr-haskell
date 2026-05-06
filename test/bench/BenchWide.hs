{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, QuasiQuotes
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, TemplateHaskell #-}
module BenchWide where
import Language.ANTLR4

[g4|

grammar BenchWide;

start : r0 ;

r0 : r1 | r2 | r3 | r4 | A | B | 'a' | 'A' ;
r1 : r2 | r3 | r4 | r5 | A | B | 'b' | 'B' ;
r2 : r3 | r4 | r5 | r6 | A | B | 'c' | 'C' ;
r3 : r4 | r5 | r6 | r7 | A | B | 'd' | 'D' ;
r4 : r5 | r6 | r7 | r8 | A | B | 'e' | 'E' ;
r5 : r6 | r7 | r8 | r9 | A | B | 'f' | 'F' ;
r6 : r7 | r8 | r9 | r10 | A | B | 'g' | 'G' ;
r7 : r8 | r9 | r10 | r11 | A | B | 'h' | 'H' ;
r8 : r9 | r10 | r11 | r12 | A | B | 'i' | 'I' ;
r9 : r10 | r11 | r12 | r13 | A | B | 'j' | 'J' ;
r10 : r11 | r12 | r13 | r14 | A | B | 'k' | 'K' ;
r11 : r12 | r13 | r14 | r15 | A | B | 'l' | 'L' ;
r12 : r13 | r14 | r15 | r16 | A | B | 'm' | 'M' ;
r13 : r14 | r15 | r16 | r17 | A | B | 'n' | 'N' ;
r14 : r15 | r16 | r17 | r18 | A | B | 'o' | 'O' ;
r15 : r16 | r17 | r18 | r19 | A | B | 'p' | 'P' ;
r16 : r17 | r18 | r19 | r20 | A | B | 'q' | 'Q' ;
r17 : r18 | r19 | r20 | r21 | A | B | 'r' | 'R' ;
r18 : r19 | r20 | r21 | r22 | A | B | 's' | 'S' ;
r19 : r20 | r21 | r22 | r23 | A | B | 't' | 'T' ;
r20 : r21 | r22 | r23 | r24 | A | B | 'u' | 'U' ;
r21 : r22 | r23 | r24 | r25 | A | B | 'v' | 'V' ;
r22 : r23 | r24 | r25 | r26 | A | B | 'w' | 'W' ;
r23 : r24 | r25 | r26 | r27 | A | B | 'x' | 'X' ;
r24 : r25 | r26 | r27 | r28 | A | B | 'y' | 'Y' ;
r25 : r26 | r27 | r28 | r29 | A | B | 'z' | 'Z' ;
r26 : r27 | r28 | r29 | r0 | A | B | 'a' | 'A' ;
r27 : r28 | r29 | r0 | r1 | A | B | 'b' | 'B' ;
r28 : r29 | r0 | r1 | r2 | A | B | 'c' | 'C' ;
r29 : r0 | r1 | r2 | r3 | A | B | 'd' | 'D' ;

A : 'a' ;
B : 'b' ;
WS : [ \t\n\r]+ ;
|]
