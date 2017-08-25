{-# LANGUAGE QuasiQuotes, DeriveAnyClass, DeriveGeneric, TypeFamilies
    , DataKinds, ScopedTypeVariables, OverloadedStrings #-}
module Test.Language.ANTLR4.G4 where

import Text.ANTLR.Grammar
import Text.ANTLR.Parser
import qualified Text.ANTLR.LR as P
--import Language.Chisel.Tokenizer
import qualified Text.ANTLR.Lex.Tokenizer as T
import qualified Text.ANTLR.Set as S
import Text.ANTLR.Set (Hashable(..), Generic(..))
import Text.ANTLR.Pretty
import Control.Arrow ( (&&&) )
import Text.ANTLR.Lex.Regex

import Language.ANTLR4

--g4_basic =
[antlr4|
  grammar G4Basic;
  exp : '1'
      | '2'
      | '3'
      ;
|]

