{-#LANGUAGE QuasiQuotes, TemplateHaskell#-}
module Language.ANTLR4.Boot.Parser where
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
import Text.ParserCombinators.Parsec.Language (haskellStyle, reservedOpNames, reservedNames
  , commentLine, commentStart, commentEnd)
import Text.ParserCombinators.Parsec.Pos      (newPos)
-- text munging
import Data.Char

import Language.ANTLR4.Boot.Syntax
import Language.ANTLR4.Regex (Regex(..), parseRegex, regexP)

traceM s = D.traceM ("[ANTLR4.Boot.Parser] " ++ s)
--traceM = return

------------------------------------------------------------------------------
-- Or-Try Combinator (tries two parsers, one after the other)
(<||>) a b = try a <|> try b

parseANTLR :: SourceName -> Line -> Column -> String -> Either ParseError [G4]
parseANTLR fileName line column input =
  PP.parse result fileName input
  where
    
    result = do
      setPosition (newPos fileName line column)
      whiteSpace
      x <- gExps
      traceM $ show x
      eof <|> errorParse
      return x
    
    errorParse = do
      rest <- manyTill anyToken eof
      unexpected $ '"' : rest ++ "\""

gExps :: PS.Parser [G4]
gExps = many1 gExp

gExp :: PS.Parser G4
gExp = do
  traceM "gExp"
  whiteSpace
  xs <- grammarP <||> lexerP <||> prodP
  traceM $ show xs
  return xs

grammarP :: PS.Parser G4
grammarP = do
  reserved "grammar"
  h <- upper
  t <- manyTill anyToken (reservedOp ";")
  traceM $ show $ Grammar (h : t)
  return $ Grammar (h : t)

prodP :: PS.Parser G4
prodP = do
  h <- lower
  t <- manyTill anyChar (reservedOp ":")
  traceM $ "[prodP] " ++ trim (h : t)
  rhsList <- sepBy1 rhsP (traceM "rhsList..." >> reservedOp "|")
  traceM $ "[prodP.rhsList] " ++ show rhsList
  reservedOp ";"
  traceM "prodP returning..."
  return $ Prod (trim (h : t)) rhsList
  where
    rhsP = do
      mPred <- optionMaybe predP
      traceM $ "[rhsP0] " ++ show mPred
      alphaList <- many alphaP
      traceM $ "[rhsP] " ++ show alphaList
      mMute <- optionMaybe muteP
      pDir  <- optionMaybe directiveP
      whiteSpace
      return $ PRHS alphaList mPred mMute pDir
    alphaP = termP <||> nonTermP
    termP = do
      whiteSpace
      char '\''
      traceM "[prodP.termP.s] "
      s <- manyTill anyChar $ char '\''
      whiteSpace
      traceM $ "[prodP.termP.s] " ++ show s
      return $ GTerm s
    nonTermP = do
      s <- identifier
      traceM $ "[nonTermP] " ++ s
      return $ GNonTerm s
    predP = do
      traceM "[predP]"
      reservedOp "{"
      haskellParseExpTill "}?"
    muteP = do
      traceM "[muteP]"
      reservedOp "{"
      haskellParseExpTill "}"
    directiveP = do
      whiteSpace
      symbol "->"
      whiteSpace
      str <- manyTill anyChar (char '\n')
      whiteSpace
      traceM $ "[directiveP]" ++ show str
      haskellParseExp str

-- TODO: not use getInput
rEOF = do
  y <- getInput
  return (case y of
    '-':'>':_ -> True
    ';':_     -> True
    _         -> False)

lexerP :: PS.Parser G4
lexerP = do
  mAnnot <- optionMaybe annot
  h <- upper
  t <- manyTill anyChar (reservedOp ":")
  traceM $ "Lexeme Name: " ++ (h:t)
  r <- regexP rEOF
  traceM $ "Regex: " ++ show r
  optionMaybe $ symbol "->"
  mDir <- optionMaybe $ manyTill anyToken (reservedOp ";")
  return $ Lex mAnnot (trim (h : t)) (LRHS r (trim <$> mDir))
  where
    annot = fragment -- <||> ....
    fragment = do
      reserved "fragment"
      return Fragment

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
    Left err    -> error err -- PP.parserZero
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
 { reservedOpNames = [";", "|", ":", "{", "}", "}?", "'"]
 , reservedNames   = ["grammar"]
 , commentLine     = "//"
 , commentStart    = "/*"
 , commentEnd      = "*/"
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
symbol        = PT.symbol      lexer

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
