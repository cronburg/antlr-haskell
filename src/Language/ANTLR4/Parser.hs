{-#LANGUAGE QuasiQuotes, TemplateHaskell#-}
module Language.ANTLR4.Parser where
-- syntax (Exp)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Debug.Trace as D (trace, traceM)

import qualified Language.Haskell.Meta as LHM

-- monadic ops
import Control.Monad (mapM)
-- parsec
import Text.ParserCombinators.Parsec
import qualified Text.Parsec.String     as PS
import qualified Text.Parsec.Prim       as PP
import qualified Text.Parsec.Token      as PT
import qualified Text.Parsec.Expr       as PE
import qualified Text.Parsec.Combinator as PC
import Text.ParserCombinators.Parsec.Language (haskellStyle, reservedOpNames, reservedNames)
import Text.ParserCombinators.Parsec.Pos      (newPos)
-- text munging
import Data.Char

import Language.ANTLR4.Syntax
import Language.ANTLR4.Regex (Regex(..), parseRegex, regexP)

--traceM s = D.traceM ("[Regex] " ++ s)
traceM = return

------------------------------------------------------------------------------
-- Or-Try Combinator (tries two parsers, one after the other)
(<||>) a b = try a <|> try b

parseANTLR :: SourceName -> Line -> Column -> String -> Either ParseError [G4]
parseANTLR fileName line column input =
  PP.parse result fileName input
  where
    result = do {
      setPosition (newPos fileName line column)
    ; whiteSpace
    ; x <- gExps
    ; whiteSpace
    ; eof <|> errorParse
    ; return x
    }
    errorParse = do{
      rest <- manyTill anyToken eof
    ; unexpected $ "<START>" ++ rest ++ "<END>"}

gExps :: PS.Parser [G4]
gExps = many1 gExp

gExp :: PS.Parser G4
gExp = do
  traceM "gExp"
  grammarP <||> lexerP <||> prodP

grammarP :: PS.Parser G4
grammarP = do
  whiteSpaceOrComment
  reserved "grammar"
  whiteSpaceOrComment
  h <- upper
  t <- manyTill anyToken (reservedOp ";")
  traceM $ show $ Grammar (h : t)
  return $ Grammar (h : t)

prodP :: PS.Parser G4
prodP = do
  whiteSpaceOrComment
  h <- lower
  t <- manyTill anyChar (reservedOp ":")
  whiteSpaceOrComment
  rhsList <- sepBy1 rhsP (reservedOp "|")
  reservedOp ";"
  return $ Prod (trim (h : t)) rhsList
  where
    rhsP = do
      whiteSpaceOrComment
      mPred <- optionMaybe predP
      alphaList <- many alphaP
      mMute <- optionMaybe muteP
      return $ PRHS alphaList mPred mMute
    alphaP = termP <||> nonTermP
    termP = do
      whiteSpaceOrComment
      reservedOp "'"
      s <- manyTill anyToken $ reservedOp "'"
      return $ Left $ GTerm s
    nonTermP = do
      whiteSpaceOrComment
      s <- identifier
      return $ Right $ GNonTerm s
    predP = do
      whiteSpaceOrComment
      reservedOp "{"
      e <- haskellParseExpTill "}?"
      return e
    muteP = do
      whiteSpaceOrComment
      reservedOp "{"
      e <- haskellParseExpTill "}"
      return e

rEOF = do
  c <- try (lookAhead (reservedOp "->" <||> reservedOp ";"))
  return True

lexerP :: PS.Parser G4
lexerP = do
  whiteSpaceOrComment
  mAnnot <- optionMaybe annot
  h <- upper
  t <- manyTill anyChar (reservedOp ":")
  traceM $ "Lexeme Name: " ++ (h:t)
  r <- regexP rEOF
  traceM $ "Regex: " ++ show r
  optionMaybe $ reservedOp "->"
  mDir <- optionMaybe $ manyTill anyToken (reservedOp ";")
  return $ Lex mAnnot (trim (h : t)) (LRHS r mDir)
  where
    annot = fragment -- <||> ....
    fragment = do
      whiteSpaceOrComment
      reserved "fragment"
      return Fragment

{-
  (reg, mDir) <- withDir <||> withoutDir
  reg' <- case parseRegex reg of
    Left  _ -> unexpected "MATT" 
    Right r -> return r
  return $ Lex mAnnot (trim (h : t)) (LRHS reg' mDir)
  where
    withDir = do
      whiteSpaceOrComment
      r <- manyTill anyToken (reservedOp "->")
      dir <- manyTill anyToken (reservedOp ";")
      return (r, Just dir)
    withoutDir = do
      whiteSpaceOrComment
      r <- manyTill anyToken (reservedOp ";")
      return (r, Nothing)
-}


-- Parser combinators end
haskellParseExpTill :: String -> PS.Parser Exp
haskellParseExpTill op = do {
                              _ <- whiteSpace
                            ; str <- manyTill anyChar (reservedOp op)
                            ; haskellParseExp str
                            }
haskellParseExp :: String -> PS.Parser Exp
haskellParseExp str =
  case LHM.parseExp str of
    Left err    -> PP.parserZero
    Right expTH -> return expTH

whiteSpaceOrComment = comment <||> whiteSpace
 where
   comment = do
     whiteSpace
     reservedOp "//"
     (manyTill anyChar $ try $ string "\n") <||> (manyTill anyChar $ try $ string "\r")
     return ()

------------------------------------------------------------------------------
-- Lexer
lexer :: PT.TokenParser ()
lexer = PT.makeTokenParser $ haskellStyle
 { reservedOpNames = ["//",";","|", ":", "{", "}", "}?", "'", "->"]
 , reservedNames   = ["grammar"]
 }

whiteSpace    = PT.whiteSpace  lexer
identifier    = PT.identifier  lexer
operator      = PT.operator    lexer
reserved      = PT.reserved    lexer
reservedOp    = PT.reservedOp  lexer
charLiteral   = PT.charLiteral lexer
stringLiteral = PT.stringLiteral  lexer
integer       = PT.integer     lexer
natural       = PT.natural     lexer
commaSep1     = PT.commaSep1   lexer
parens        = PT.parens      lexer
braces        = PT.braces      lexer
brackets      = PT.brackets    lexer


expr = PE.buildExpressionParser table term
      <?> "expression"
term = natural
      <?> "simple expression"
table = [ [prefix "-" negate, prefix "+" id ] ]
prefix   name fun = PE.Prefix $ reservedOp name >> return fun

-- http://stackoverflow.com/a/6270382
trim xs = dropSpaceTail "" $ dropWhile isSpace xs

dropSpaceTail maybeStuff "" = ""
dropSpaceTail maybeStuff (x:xs)
        | isSpace x = dropSpaceTail (x:maybeStuff) xs
        | null maybeStuff = x : dropSpaceTail "" xs
        | otherwise       = reverse maybeStuff ++ x : dropSpaceTail "" xs
