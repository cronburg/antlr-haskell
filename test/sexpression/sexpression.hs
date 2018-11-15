module Main where
import Language.ANTLR4
import Grammar

main =
  case glrParse isWS "((m1lk ju1ce) (h0ney marmalade))" of
    (ResultAccept ast) -> print $ ast2sexpr ast
    x                  -> print x

