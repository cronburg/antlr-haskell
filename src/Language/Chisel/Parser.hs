module Language.Chisel.Parser
  ( parse
  ) where

import qualified Text.ANTLR.LR1 as P
import Language.Chisel.Tokenizer
import Text.ANTLR.AST

type ChiselNT = String
type ChiselT  = String
type ChiselAST = AST ChiselNT ChiselT

parse :: [Icon ChiselT] -> Maybe ChiselAST
parse w = P.slrParse chiselGrammar event2ast

