{-# LANGUAGE QuasiQuotes #-}
module Test.Language.ANTLR4.G4 where

import Language.ANTLR4

g4_basic =
  [antlr4|
    grammar Test;
    exp : '1'
        | '2'
        | '3'
        ;
  |]
