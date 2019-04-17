{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, QuasiQuotes
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, FlexibleContexts, TemplateHaskell
    , DeriveDataTypeable #-}
module DupTermsGrammar where
import Language.ANTLR4

doesThisFire :: Int
doesThisFire = 3

orDoesThisFire :: String -> Int
orDoesThisFire _ = 4

$( return [] )

[g4|
  grammar DupTerms;

  justParen : '(' -> doesThisFire ;

  LParen : '(' -> orDoesThisFire ;

  WS      : [ \t\n\r\f\v]+     -> String;
|]

