{-# LANGUAGE QuasiQuotes, DeriveAnyClass, DeriveGeneric, TypeFamilies
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, FlexibleContexts #-}
module Test.Language.ANTLR4.G4 where
import Language.ANTLR4

[g4|
  grammar G4Basic;
  exp : '1'
      | '2'
      | '3'
      ;
|]

