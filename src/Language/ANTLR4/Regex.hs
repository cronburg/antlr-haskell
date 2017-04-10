module Language.ANTLR4.Regex (Regex(..), parseRegex) where
import Text.ParserCombinators.Parsec
import qualified Text.Parsec.String     as PS
import qualified Text.Parsec.Prim       as PP
import qualified Text.Parsec.Token      as PT
import qualified Text.Parsec.Expr       as PE
import qualified Text.Parsec.Combinator as PC
import Data.Char (ord)
import Text.ParserCombinators.Parsec.Language

data Regex s =
    Epsilon
  | Literal   [s]
  | Symbol    s
  | Union     (Regex s) (Regex s)
  | Concat    (Regex s) (Regex s)
  | Kleene    (Regex s)
  | PosClos   (Regex s)
  | Question  (Regex s)
  | CharSet   [s] -- TODO: Set s, and ranges of characters
  deriving (Eq, Show)
-- TODO: Lex regexs (e.g. complement sets, escape chars, ...)

(<||>) a b = try a <|> try b

parseRegex :: String -> Either ParseError (Regex Char)
parseRegex input = PP.parse regexP "" input

type RegexC = Regex Char

regexP :: PS.Parser RegexC
regexP = charSet <||> literal

-- regex string literal uses single quotes
literal :: PS.Parser RegexC
literal = PC.between (reservedOp "'") (reservedOp "'") (many singleChar >>= (return . Literal))

charSet :: PS.Parser RegexC
charSet = PC.between (reservedOp "[") (reservedOp "]") (charSetBody >>= (return . CharSet))

charSetBody :: PS.Parser [Char]
charSetBody = do
  xs <- many $ charSetRange <||> (singleChar >>= (\c -> return [c]))
  return $ concat xs

charSetRange :: PS.Parser [Char]
charSetRange = do
  start <- singleChar
  reservedOp "-"
  end   <- singleChar
  if ord end <= ord start
    then unexpected [end]
    else return [start..end]

singleChar = satisfy ('\'' /=) <||> escapedQuote

escapedQuote :: PS.Parser Char
escapedQuote = do
  reservedOp "\\"
  reservedOp "'"
  return '\''

regexLexer :: PT.TokenParser ()
regexLexer = PT.makeTokenParser $ haskellStyle
	{ reservedOpNames = ["[", "]", "\\", "-", "+"] }

whiteSpace    = PT.whiteSpace  regexLexer
identifier    = PT.identifier  regexLexer
operator      = PT.operator    regexLexer
reserved      = PT.reserved    regexLexer
reservedOp    = PT.reservedOp  regexLexer
charLiteral   = PT.charLiteral regexLexer
stringLiteral = PT.stringLiteral  regexLexer
integer       = PT.integer     regexLexer
natural       = PT.natural     regexLexer
commaSep1     = PT.commaSep1   regexLexer
parens        = PT.parens      regexLexer
braces        = PT.braces      regexLexer
brackets      = PT.brackets    regexLexer

