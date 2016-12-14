module Text.ANTLR.Allstar.Lex where
import Text.ANTLR.Allstar.Grammar (Terminal)

type Token = String

termOf :: Token -> Terminal
termOf = id
