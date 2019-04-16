{-# LANGUAGE QuasiQuotes, DeriveAnyClass, DeriveGeneric, TypeFamilies
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, FlexibleContexts, TemplateHaskell #-}
module DoubleSemi where
import Language.ANTLR4

$( return [] )

[g4|
  grammar Dbl;
  dbl : 'a' | 'f' ; // ;
  WS  : [ \t\r\n]+ -> String;
|]

isWS T_WS = True
isWS _    = False

