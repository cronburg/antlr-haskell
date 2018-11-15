{-# LANGUAGE QuasiQuotes, DeriveAnyClass, DeriveGeneric, TypeFamilies
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, FlexibleContexts #-}
module Language.ANTLR4.Example.Optionals where
import Language.ANTLR4

opt a b c d = (1, 'b', 2.0, [1,2,3])

foo :: () -> Maybe (Int, Char, Double, [Int]) -> String
foo a1 (Just (a,b,c,d)) = "accept"
foo a1 Nothing = "reject"

[g4|
  grammar Optional;
  r   : a s? -> foo;
  s   : a? b* c+ d -> opt;
  a   : 'a';
  b   : 'b';
  c   : 'c';
  d   : 'd';
  
  ID  : [a-zA-Z]+ -> String;
  WS  : [ \t\r\n]+ -> String;
|]

isWS T_WS = True
isWS _    = False

