
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
import Test.Language.ANTLR4.G4 (g4_basic)
import Test.Language.ANTLR4.Hello (hello)
import Language.ANTLR4.Regex

test_g4_basic_type_check = do
  let _ = g4_basic
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

main :: IO ()
main = defaultMainWithOpts
  [ testCase "g4_basic_compilation_type_check" test_g4_basic_type_check
  , testCase "hello_parse_type_check" hello_g4_test_type_check
  , testCase "regex_test" regex_test
  ] mempty
