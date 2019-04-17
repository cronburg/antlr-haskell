{-# LANGUAGE QuasiQuotes, DeriveAnyClass, DeriveGeneric, TypeFamilies
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, FlexibleContexts, TemplateHaskell #-}
module ConvertDFA where
import Language.ANTLR4

mkInt :: String -> Int
mkInt _ = 3

data ConvertIt =
    Start String
  | End   Int
  deriving (Eq, Ord, Show)

$( return [] )

[g4|
  grammar Convert;

  root  : 'START' LexemeA -> Start
        | 'END'   LexemeB -> End
        ;

  LexemeA : 'abc' -> String ;
  LexemeB : 'efg' -> mkInt ;
|]

