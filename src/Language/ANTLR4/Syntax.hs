{-|
  Module      : Language.ANTLR4.Syntax
  Description : Helper syntax functions used by core G4 parser
  Copyright   : (c) Karl Cronburg, 2018
  License     : BSD3
  Maintainer  : karl@cs.tufts.edu
  Stability   : experimental
  Portability : POSIX
-}
module Language.ANTLR4.Syntax where
import Language.ANTLR4.Boot.Syntax

import qualified Debug.Trace as D

-- | Debugging support
trace s = D.trace ("Language.ANTLR4.Syntax] " ++ s)

-- | Parse an escape characters allowable in G4:
readEscape :: String -> Char
readEscape s = let
    eC ('\\':'n':xs)   = '\n'
    eC ('\\':'r':xs)   = '\r'
    eC ('\\':'t':xs)   = '\t'
    eC ('\\':'b':xs)   = '\b'
    eC ('\\':'f':xs)   = '\f'
    eC ('\\':'v':xs)   = '\v'
    eC ('\\':'"':xs)   = '\"'
    eC ('\\':'\'':xs)  = '\''
    eC ('\\':'\\':xs)  = '\\'
  in eC s

-- | Parse a literal String by stripping the quotes at the beginning and end of
--   the String, and replacing all escaped characters with the actual escape
--   character code.
stripQuotesReadEscape :: String -> String
stripQuotesReadEscape s = let

    eC [] = error "String ended in a single escape '\\': '" ++ s ++ "'"
    eC ('n':xs)   = "\n" ++ sQRE xs
    eC ('r':xs)   = "\r" ++ sQRE xs
    eC ('t':xs)   = "\t" ++ sQRE xs
    eC ('b':xs)   = "\b" ++ sQRE xs
    eC ('f':xs)   = "\f" ++ sQRE xs
    eC ('v':xs)   = "\v" ++ sQRE xs
    eC ('"':xs)   = "\"" ++ sQRE xs
    eC ('\'':xs)  = "\'" ++ sQRE xs
    eC ('\\':xs)  = "\\" ++ sQRE xs
    eC (x:xs)     = error $ "Invalid escape character '" ++ [x] ++ "' in string '" ++ s ++ "'"

    sQRE [] = []
    sQRE ('\\':xs) = eC xs
    sQRE (x:xs) = x : sQRE xs

  --in trace s $ (sQRE . init . tail) s
  in (sQRE . init . tail) s
  --read $ "\"" ++ (init . tail) s ++ "\"" :: String

