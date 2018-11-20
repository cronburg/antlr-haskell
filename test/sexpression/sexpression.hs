module Main where
import Language.ANTLR4
import Grammar
import qualified Text.ANTLR.Set as S

getAST (ResultAccept ast) = ast
getAST _ = error "non-AST in ResultSet"

main =
  case glrParse isWS "((m1lk ju1ce 3.1) . (h0ney marmalade \"jam\"))" of
    (ResultAccept ast) -> print $ ast2sexpr ast
    (ResultSet xs)     -> mapM_ (print . ast2sexpr . getAST) (S.toList xs)

