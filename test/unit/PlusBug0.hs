{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, QuasiQuotes
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, FlexibleContexts, TemplateHaskell
    , DeriveDataTypeable #-}
module PlusBug0 where
import Language.ANTLR4

data Plus = Plus [String] | NotPlus [String]
  deriving (Eq, Show)

[g4|
  grammar PlusBug0;

  plus : LowerID+ -> Plus ;

  LowerID : [a-z][a-zA-Z0-9_]* -> String;
  WS      : [ \t\n\r\f\v]+     -> String;
|]

