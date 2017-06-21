
module Main where
-- Allstar imports go here, e.g.:
-- import Text.Allstar.ATN (..)


import System.IO.Unsafe (unsafePerformIO)
import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck (Property, quickCheck, (==>))
import qualified Test.QuickCheck.Monadic as TQM

import Language.ANTLR4
import Text.ANTLR.Allstar.Grammar
import qualified Test.Language.ANTLR4.G4 as G4
import Test.Language.ANTLR4.Hello
import Language.ANTLR4.Regex
import Text.ANTLR.Parser (AST(..))
import qualified Text.ANTLR.LR as LR
import qualified Text.ANTLR.Lex.Tokenizer as T

import qualified Language.ANTLR4.G4 as P -- Parser

test_g4_basic_type_check = do
  let _ = G4.g4Basic
  1 @?= 1

hello_g4_test_type_check = do
  let _ = hello
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
  LR.ResultAccept (AST G4.Exp [T G4.T_0] [Leaf _1])
 
test_hello =
  slrParse (tokenize "hello Matt")
  @?=
  (LR.ResultAccept $
        AST R [T T_0, T T_WS, T T_ID]
        [ Leaf (T.Token T_0 V_0)
        , Leaf (T.Token T_WS (V_WS " "))
        , Leaf (T.Token T_ID (V_ID "Matt"))
        ]
  )

main :: IO ()
main = defaultMainWithOpts
  [ testCase "g4_basic_compilation_type_check" test_g4_basic_type_check
  , testCase "hello_parse_type_check" hello_g4_test_type_check
  , testCase "regex_test" regex_test
  , testCase "test_g4" test_g4
  , testCase "test_hello" test_hello
  ] mempty

