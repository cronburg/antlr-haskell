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


-- https://github.com/antlr/grammars-v4/blob/master/antlr4/examples/Hello.g4
hello =
  [antlr4|
    grammar Hello;
    r   : 'hello' ID;
    ID  : [a-z]+ ;
    WS  : [ \t\r\n]+ -> skip ;
  |]

