{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, QuasiQuotes
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, FlexibleContexts, TemplateHaskell
    , DeriveDataTypeable #-}
module Star1Grammar where
import Language.ANTLR4

data MyMaybe x = Nope | Yep x
  deriving (Eq, Ord, Show)

data Mem = Byte
  deriving (Eq, Ord, Show)

data Words =
    Frst (MyMaybe (String, String)) [String] [Mem]
  | Snd
  | Thrd (MyMaybe (String, String))
  deriving (Eq, Ord, Show)

[g4|
  grammar Star1;

  words   : me 'you' page* '{' bytes '}'  -> Frst
          | me                            -> Thrd
          | 'woops'                       -> Snd
          ;

  bytes : 'byte'            -> ${ [Byte] }
        | bytes ',' 'byte'  -> ${\bs -> Byte : bs}
        ;

  me  : me2? -> ${\m -> case m of Nothing -> Nope ; Just x -> Yep x};
  me2 : me3 ;
  me3 : Me page ;
  Me  : 'me' -> String ;

  page    : Page ;
  Page : 'page' -> String ;

  WS      : [ \t\n\r\f\v]+     -> String;
|]

