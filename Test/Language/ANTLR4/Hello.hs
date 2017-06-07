{-# LANGUAGE QuasiQuotes, DeriveAnyClass, DeriveGeneric, TypeFamilies
		, DataKinds, ScopedTypeVariables #-}
module Test.Language.ANTLR4.Hello where

import Text.ANTLR.Allstar.Grammar
import Text.ANTLR.Parser
import qualified Text.ANTLR.LR1 as P
--import Language.Chisel.Tokenizer
import qualified Text.ANTLR.Lex.Tokenizer as T
import qualified Text.ANTLR.Set as S
import Text.ANTLR.Set (Hashable(..), Generic(..))
import Text.ANTLR.Pretty
import Control.Arrow ( (&&&) )
import Text.ANTLR.Lex.Regex

import Language.ANTLR4

[antlr4|
	// Hello World grammar
	// https://github.com/antlr/grammars-v4/blob/master/antlr4/examples/Hello.g4
	grammar Hello;
	r   : 'hello' ID;
	ID  : [a-z]+ -> String;
	WS  : [ \t\r\n]+ -> String;
|]

