{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, QuasiQuotes
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, FlexibleContexts, TemplateHaskell
    , DeriveDataTypeable #-}
module GLRPartialGrammar where
import Language.ANTLR4

[g4|
  grammar GLRPartial;

  words : word+ ;

  word : Prim ;

  LowerID : [a-z][a-zA-Z0-9_]* -> String;
  Prim    : 'word'             -> String;

  WS      : [ \t\n\r\f\v]+     -> String;
|]

