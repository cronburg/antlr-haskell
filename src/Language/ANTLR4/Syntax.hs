module Language.ANTLR4.Syntax where
import Language.ANTLR4.Boot.Syntax
import Language.ANTLR4.Regex (Regex(..))

import qualified Debug.Trace as D

trace s = D.trace ("Language.ANTLR4.Syntax] " ++ s)

append :: String -> String -> String
append = (++)

list a = [a]
cons = (:)
lexemeDirective r d = LRHS r (Just d)
lexemeNoDir     r   = LRHS r Nothing
lexDecl = Lex Nothing
lexFragment = Lex (Just Fragment)

literalRegex :: String -> Regex Char
literalRegex = Literal

prodDirective as d = PRHS as Nothing Nothing (Just d)
prodNoDir     as   = PRHS as Nothing Nothing Nothing

list2 a b = [a,b]
range a b = [a .. b]

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
--read $ "'" ++ s ++ "'"

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

char :: String -> Char
char = head

