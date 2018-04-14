{-# LANGUAGE QuasiQuotes, DeriveAnyClass, DeriveGeneric, TypeFamilies
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, FlexibleContexts #-}
module Language.ANTLR4.Example.Optionals where
import Language.ANTLR4

opt a b c d = ()

--opt (Just a) b c (Just d) = "opt1"
--opt (Just a) b c Nothing  = "opt2"
--opt Nothing  b c (Just d) = "opt3"
--opt Nothing  b c Nothing  = "opt4"

[g4|
  grammar Optional;
  r   : a? b c d?    -> opt;
  a   : 'a';
  b   : 'b';
  c   : 'c';
  d   : 'd';
  
  ID  : [a-zA-Z]+ -> String;
  WS  : [ \t\r\n]+ -> String;
|]

isWS T_WS = True
isWS _    = False

