{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, QuasiQuotes
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, DeriveDataTypeable
    , TemplateHaskell #-}
{-|
  Module      : Language.ANTLR4.Parser
  Description : Core G4 quasiquoter (with parsers) for antlr-haskell
  Copyright   : (c) Karl Cronburg, 2019
  License     : BSD3
  Maintainer  : karl@cs.tufts.edu
  Stability   : experimental
  Portability : POSIX

-}
module Language.ANTLR4.Parser (g4) where

import Control.Arrow ( (&&&) )
import Data.Char (isUpper)

import Text.ANTLR.Common
import Text.ANTLR.Grammar
import Text.ANTLR.Parser
import qualified Text.ANTLR.LR as LR
import Text.ANTLR.Lex.Tokenizer as T hiding (tokenize)
import qualified Text.ANTLR.Set as S
import Text.ANTLR.Set (Hashable(..), Generic(..))
import Text.ANTLR.Pretty
import Text.ANTLR.Lex.Regex (regex2dfa)
import Data.Data (Data(..))
import Language.Haskell.TH.Lift (Lift(..))

import Language.Haskell.TH.Quote (QuasiQuoter(..))
import qualified Language.Haskell.TH as TH
import Language.ANTLR4.Boot.Quote (antlr4, g4_parsers)
import Language.ANTLR4.Syntax
import qualified Language.ANTLR4.Boot.Syntax  as G4S
import qualified Language.ANTLR4.Boot.Quote   as G4Q
import Language.ANTLR4.G4

import Debug.Trace as D
import System.CPUTime (getCPUTime)
import Control.Exception (evaluate)
import System.IO (hFlush, stdout)

-- Splice the parsers for the grammar we defined in Language.ANTLR4.G4
$(g4_parsers g4AST g4Grammar)

{- isWhitespace (Just T_LineComment) = True
isWhitespace (Just T_WS) = True
isWhitespace Nothing = True
isWhitespace _ = False -}

isWhitespace T_LineComment = True
isWhitespace T_WS = True
isWhitespace _ = False

-- | Cached G4 parser: full GLR with error-pruning (glrParseInc2). The left-
-- recursive unionR G4 grammar fix eliminates the dominant Reduce/Reduce source,
-- making error-pruned GLR practical. Tables computed once as a CAF.
{-# NOINLINE g4ParseCached #-}
g4ParseCached :: LR.Tokenizer G4Token Char -> [Char] -> LR.GLRResult Int Char G4Token G4AST
g4ParseCached = LR.glrParseInc2 g4Grammar event2ast

showTime :: Integer -> Integer -> String
showTime t0 t1 = show (fromIntegral (t1 - t0) / 1e12 :: Double) ++ "s"

timed :: String -> TH.Q a -> TH.Q a
timed label action = do
  t0 <- TH.runIO getCPUTime
  result <- action
  t1 <- TH.runIO getCPUTime
  TH.runIO $ putStrLn $ "[g4 timing] " ++ label ++ ": " ++ showTime t0 t1
  TH.runIO $ hFlush stdout
  return result


g4_codeGen :: String -> TH.Q [TH.Dec]
g4_codeGen input = do
  loc <- TH.location
  let fileName = TH.loc_filename loc
  let (line,column) = TH.loc_start loc
  TH.runIO $ putStrLn $ "[g4 timing] --- " ++ fileName ++ ":" ++ show line
    ++ " (" ++ show (length input) ++ " chars)"
  TH.runIO $ hFlush stdout
  parsed <- timed "glrParse" $ TH.runIO $ evaluate $
    g4ParseCached (T.tokenizeInc isWhitespace g4DFAs lexeme2value) input
  timed "g4_decls" $ case parsed of
    (LR.ResultAccept ast) -> codeGen ast
    LR.ResultSet    s   ->
      if S.size s == 1
        then codeGen $ fromAccept (S.findMin s)
        else D.trace (pshow' s) $ codeGen $ fromAccept (S.findMin s)
    err -> error $ pshow' err

fromAccept (LR.ResultAccept ast) = ast
fromAccept err = error $ pshow' err

codeGen ast = G4Q.g4_decls $ ast2decls ast

-- | Entrypoint to the G4 quasiquoter. Currently only supports declaration-level
--   Haskell generation of G4 grammars using a GLR parser. The output grammars
--   need not use a GLR parser themselves.
g4 :: QuasiQuoter
g4 = QuasiQuoter
  (error "parse exp")
  (error "parse pattern")
  (error "parse type")
  g4_codeGen

