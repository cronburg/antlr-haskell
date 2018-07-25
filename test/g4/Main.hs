{-# LANGUAGE TemplateHaskell #-}
module Main where

import System.IO.Unsafe (unsafePerformIO)
import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck (Property, quickCheck, (==>))
import qualified Test.QuickCheck.Monadic as TQM

import Language.ANTLR4 hiding (tokenize, Regex(..))
import Text.ANTLR.Grammar
import qualified Language.ANTLR4.Example.Optionals as Opt
import qualified Language.ANTLR4.Example.G4 as G4
import Language.ANTLR4.Example.G4 (g4BasicGrammar, G4BasicNTSymbol, G4BasicTSymbol, G4BasicAST)
import Language.ANTLR4.Example.Hello
import Language.ANTLR4.Regex
import Text.ANTLR.Parser (AST(..))
import qualified Text.ANTLR.LR as LR
import qualified Text.ANTLR.Lex.Tokenizer as T

import qualified Language.ANTLR4.G4 as P -- Parser

import qualified G4 as Fast

test_g4_basic_type_check = do
  let _ = G4.g4BasicGrammar
  1 @?= 1

hello_g4_test_type_check = do
  let _ = helloGrammar
  1 @?= 1

regex_test = do
  parseRegex "[ab]* 'a' 'b' 'b'"
  @?= Right
  (Concat
    [ Kleene $ CharSet "ab"
    , Literal "a"
    , Literal "b"
    , Literal "b"
    ])

_1 = G4.lookupToken "1"

-- TODO: implement 'read' instance for TokenValue type so that I don't have to
-- hardcode the name for literal terminals (e.g. '1' == T_0 below)
test_g4 =
  G4.slrParse (G4.tokenize "1")
  @?=
  LR.ResultAccept (AST G4.NT_exp [T G4.T_0] [Leaf _1])
 
test_hello =
  slrParse (tokenize "hello Matt")
  @?=
  (LR.ResultAccept $
        AST NT_r [T T_0, T T_WS, T T_ID]
        [ Leaf (T.Token T_0 V_0 1)
        , Leaf (T.Token T_WS (V_WS " ") 1)
        , Leaf (T.Token T_ID (V_ID "Matt") 4)
        ]
  )

test_hello_allstar =
  allstarParse (tokenize "hello Matt")
  @?=
  Right (AST NT_r [] [Leaf (Token T_0 V_0 5),Leaf (Token T_WS (V_WS " ") 1),Leaf
  (Token T_ID (V_ID "Matt") 4)])
  --Right (AST NT_r [] [])

test_optional =
  Opt.glrParse Opt.isWS "a"
  @?=
  (LR.ResultAccept $ AST Opt.NT_r [NT Opt.NT_a] [AST Opt.NT_a [T Opt.T_0] [Leaf (Token Opt.T_0 Opt.V_0 1)]])

test_optional2 =
  case Opt.glrParse Opt.isWS "a a b c d" of
    LR.ResultAccept ast -> Opt.ast2r ast @?= "accept"
    err                 -> error $ show err

test_optional3 =
  case Opt.glrParse Opt.isWS "a b b b c c d" of
    LR.ResultAccept ast -> Opt.ast2r ast @?= "accept"
    err                 -> error $ show err

test_optional4 =
  case Opt.glrParse Opt.isWS "a" of
    LR.ResultAccept ast -> Opt.ast2r ast @?= "reject"
    err                 -> error $ show err

testFastGLR =
  Fast.glrParseFast (const False) "3"
  @?=
  G4.glrParse (const False) "3"

main :: IO ()
main = defaultMainWithOpts
  [ testCase "g4_basic_compilation_type_check" test_g4_basic_type_check
  , testCase "hello_parse_type_check" hello_g4_test_type_check
  , testCase "regex_test" regex_test
  , testCase "test_g4" test_g4
  , testCase "test_hello" test_hello
  , testCase "test_hello_allstar" test_hello_allstar
  , testCase "test_optional" test_optional
  , testCase "test_optional2" test_optional2
  , testCase "test_optional3" test_optional3
  , testCase "test_optional4" test_optional4
  , testCase "testFastGLR" testFastGLR
  ] mempty

