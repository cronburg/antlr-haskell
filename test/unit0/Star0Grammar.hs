{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, QuasiQuotes
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, FlexibleContexts, TemplateHaskell
    , DeriveDataTypeable #-}
module Star0Grammar where
import Language.ANTLR4

[g4|
  grammar Star0;

  words   : page* -> ${\ps -> ps} ;

  page    : Page ;

  Page : 'page' -> String ;

  WS      : [ \t\n\r\f\v]+     -> String;
|]

