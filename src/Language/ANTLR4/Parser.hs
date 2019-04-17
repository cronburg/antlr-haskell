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

-- Splice the parsers for the grammar we defined in Language.ANTLR4.G4
$(g4_parsers g4AST g4Grammar)

{- isWhitespace (Just T_LineComment) = True
isWhitespace (Just T_WS) = True
isWhitespace Nothing = True
isWhitespace _ = False -}

isWhitespace T_LineComment = True
isWhitespace T_WS = True
isWhitespace _ = False

g4_codeGen :: String -> TH.Q [TH.Dec]
g4_codeGen input = do
  loc <- TH.location
  let fileName = TH.loc_filename loc
  let (line,column) = TH.loc_start loc

  {- case allstarParse (filter (not . isWhitespace . stripEOF . getSymbol) (tokenize input)) of
    Left err -> error err
    Right ast -> codeGen ast -}
  case glrParse isWhitespace input of
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

