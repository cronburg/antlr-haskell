{-# LANGUAGE QuasiQuotes, DeriveAnyClass, DeriveGeneric, TypeFamilies
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, FlexibleContexts, TemplateHaskell #-}
module Empty where
import Language.ANTLR4

$( return [] )

[g4|
  grammar Empty;
  emp : 'a' b | 'f' d ;
  b   : 'c'
      | // empty!
      ;

  d   : // empty!
      | 'd'
      ;

  WS  : [ \t\r\n]+ -> String;
|]

isWS T_WS = True
isWS _    = False

