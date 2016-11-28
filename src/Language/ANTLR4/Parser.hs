{-#LANGUAGE QuasiQuotes, TemplateHaskell#-}
module Language.ANTLR4.Parser where
-- syntax (Exp)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Debug.Trace (trace)

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



-- .g4 style syntax
data G4 = Grammar {gName :: String}
        | Prod {pName :: String, patterns :: [PRHS] }
        | Lex  {annotation :: Maybe GAnnot, lName :: String, pattern :: LRHS }
  deriving (Show, Eq)

data PRHS     = PRHS { alphas :: [Either GTerm GNonTerm]
                        , pred :: Maybe Exp
                        , mutator :: Maybe Exp
                        }
  deriving (Show, Eq)

newtype GTerm    = GTerm String
  deriving (Show,Eq)
newtype GNonTerm = GNonTerm String
  deriving (Show, Eq)
data    GAnnot   = Fragment
  deriving (Show, Eq)

data LRHS     = LRHS { regex :: String
                     , directive :: Maybe String
                     }
  deriving (Show, Eq)

-- Parser
sE :: String -> Exp
sE = LitE . StringL

msE :: Maybe String -> Exp
msE Nothing = ConE $ mkName "Nothing"
msE (Just s) = AppE (ConE $ mkName "Just") (sE s)
-- Lifting makes quasiquotation easier
instance Lift G4 where
  lift (Grammar n)  = [|Grammar $(return $ sE n)|]
  lift (Prod n rhs) = do
    rhsExps <- mapM lift rhs
    [|Prod $(return $ sE n) $(return $ ListE rhsExps)|]
  lift (Lex Nothing n rhs)  =  [|Lex Nothing $(return $ sE n) rhs   |]
  lift (Lex (Just Fragment) n rhs) =  [|Lex (Just Fragment) $(return $ sE n) rhs |]

instance Lift PRHS where
  lift (PRHS alps mpred mmut) = do
    alpsE <- mapM lAlps alps
    [|PRHS $(return $ ListE $ alpsE) $(lMExp mpred) $(lMExp mmut)|]
    where
      lAlps (Left (GTerm s))     = [| Left $ GTerm $(return $ sE s)|]
      lAlps (Right (GNonTerm s)) = [| Right $ GNonTerm $(return $ sE s)|]
      lMExp Nothing              = [| Nothing|]
      lMExp (Just e)             = [| Just $(return e)|]

instance Lift LRHS where
  lift (LRHS r md) = [| LRHS $(return $ sE r) $(return $ msE md)|]

-- Parser combinators begin

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
    ; unexpected rest }

gExps :: PS.Parser [G4]
gExps = many gExp

gExp :: PS.Parser G4
gExp = grammarP <||> prodP <||> lexerP

grammarP :: PS.Parser G4
grammarP = do
  whiteSpaceOrComment
  reserved "grammar"
  whiteSpaceOrComment
  h <- upper
  t <- manyTill anyToken (reservedOp ";")
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

lexerP :: PS.Parser G4
lexerP = do
  whiteSpaceOrComment
  mAnnot <- optionMaybe annot
  h <- upper
  t <- manyTill anyChar (reservedOp ":")
  (reg, mDir) <- withDir <||> withoutDir
  return $ Lex mAnnot (trim (h : t)) (LRHS reg mDir)
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
    annot = fragment -- <||> ....
    fragment = do
      whiteSpaceOrComment
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
