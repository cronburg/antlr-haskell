{-# LANGUAGE TemplateHaskell, FlexibleContexts, TypeFamilies #-}
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
--import Language.ANTLR4.Regex
import Text.ANTLR.Parser (AST(..))
import qualified Text.ANTLR.LR as LR
import qualified Text.ANTLR.Lex.Tokenizer as T

import qualified Language.ANTLR4.G4 as P -- Parser

import qualified Optional as Opt
import qualified OptionalParser as Opt

import qualified EmptyP as E
import qualified Empty  as E

test_optional =
  Opt.glrParse Opt.isWS "a"
  @?=
  (LR.ResultAccept $ AST Opt.NT_r [NT Opt.NT_a] [AST Opt.NT_a [T Opt.T_1] [Leaf (Token Opt.T_1 Opt.V_1 1)]])

test_optional2 =
  case Opt.glrParse Opt.isWS "a a e b c d" of
    LR.ResultAccept ast -> Opt.ast2r ast @?= "accept"
    err                 -> error $ show err

test_optional3 =
  case Opt.glrParse Opt.isWS "a e b b b c c d" of
    LR.ResultAccept ast -> Opt.ast2r ast @?= "accept"
    err                 -> error $ show err

test_optional4 =
  case Opt.glrParse Opt.isWS "a" of
    LR.ResultAccept ast -> Opt.ast2r ast @?= "reject"
    err                 -> error $ show err

test_e v =
  case v of
    LR.ResultAccept ast -> E.ast2emp ast @?= ()
    err                 -> error $ show err
test_e_fail v =
  case v of
    LR.ResultAccept ast -> assertFailure $ show ast
    err                 -> () @?= ()
  
test_empty = test_e (E.slrParse (E.tokenize "a"))
test_empty2 = test_e (E.slrParse (E.tokenize "f"))
test_empty3 = test_e (E.slrParse (E.tokenize "ac"))
test_empty4 = test_e (E.slrParse (E.tokenize "fd"))
test_empty5 = test_e_fail (E.slrParse (E.tokenize "fc"))
test_empty6 = test_e_fail (E.slrParse (E.tokenize "ad"))

main :: IO ()
main = defaultMainWithOpts
  [ testCase "test_optional" test_optional
  , testCase "test_optional2" test_optional2
  , testCase "test_optional3" test_optional3
  , testCase "test_optional4" test_optional4
  , testCase "test_empty"     test_empty
  , testCase "test_empty2"    test_empty2
  , testCase "test_empty3"    test_empty3
  , testCase "test_empty4"    test_empty4
  , testCase "test_empty5"    test_empty5
  , testCase "test_empty6"    test_empty6
  ] mempty

