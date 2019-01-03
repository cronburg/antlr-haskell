{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, QuasiQuotes
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, FlexibleContexts, TemplateHaskell
    , DeriveDataTypeable #-}
module GLRIncGrammar where
import Language.ANTLR4

data Plus = Plus String String | Minus String String
  deriving (Eq, Show)

[g4|
  grammar GLRInc;

  plus  : LowerID '+' Prim     -> Plus
        | Prim    '-' LowerID  -> Minus
        ;

  LowerID : [a-z][a-zA-Z0-9_]* -> String;
  Prim    : 'word'             -> String;

  WS      : [ \t\n\r\f\v]+     -> String;
|]

