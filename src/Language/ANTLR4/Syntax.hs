module Language.ANTLR4.Syntax where
import Language.ANTLR4.Boot.Syntax
import Language.ANTLR4.Regex (Regex(..))

append :: String -> String -> String
append = (++)

list a = [a]
cons = (:)
lexemeDirective r d = LRHS r (Just d)
lexemeNoDir     r   = LRHS r Nothing
lexDecl = Lex Nothing

literalRegex :: String -> Regex Char
literalRegex = Literal

prodDirective as d = PRHS as Nothing Nothing (Just d)
prodNoDir     as   = PRHS as Nothing Nothing Nothing

list2 a b = [a,b]
range a b = [a .. b]

readEscape :: String -> Char
readEscape s = read $ "'" ++ s ++ "'"

stripQuotes :: String -> String
stripQuotes = init . tail

char :: String -> Char
char = head

