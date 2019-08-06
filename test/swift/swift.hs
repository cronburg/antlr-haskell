module Main where
import Language.ANTLR4
import Parser
import qualified Text.ANTLR.Set as S

getAST (ResultAccept ast) = ast
getAST _ = error "non-AST in ResultSet"

main =
  case glrParse isWS "var i = 0;" of
    (ResultAccept ast) -> print $ ast2topLevel ast
    (ResultSet xs)     -> mapM_ (print . ast2topLevel . getAST) (S.toList xs)

