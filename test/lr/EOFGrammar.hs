{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, QuasiQuotes
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, FlexibleContexts, TemplateHaskell
    , DeriveDataTypeable #-}
module EOFGrammar where
import Language.ANTLR4

data Plus = Plus String String | Minus String String
  deriving (Eq, Show)

[g4|
  grammar EOF;

  words : word* ;

  word : WORD ;

  WORD : (~ [, ])* -> String;
  
  WS      : [ \t\n\r\f\v]+     -> String;
|]

