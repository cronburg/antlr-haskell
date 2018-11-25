{-# LANGUAGE DeriveLift, DeriveGeneric, DeriveAnyClass #-}
{-|
  Module      : Language.ANTLR4.Regex
  Description : Parsec parser for G4 regular expressions
  Copyright   : (c) Karl Cronburg, 2018
  License     : BSD3
  Maintainer  : karl@cs.tufts.edu
  Stability   : experimental
  Portability : POSIX
-}
module Language.ANTLR4.Regex (parseRegex, regexP) where
import Language.Haskell.TH.Lift (Lift(..))
import Text.ParserCombinators.Parsec
import qualified Text.Parsec.String     as PS
import qualified Text.Parsec.Prim       as PP
import qualified Text.Parsec.Token      as PT
import qualified Text.Parsec.Expr       as PE
import qualified Text.Parsec.Combinator as PC
import Data.Char (ord)
import Text.ParserCombinators.Parsec.Language
import qualified Debug.Trace as D -- trace, traceM

import Text.ANTLR.Set ( Hashable(..), Generic(..) )
import Language.ANTLR4.Boot.Syntax ( Regex(..) )

--traceM s = D.traceM ("[Regex] " ++ s)
traceM = return

(<||>) a b = try a <|> try b

rEOF' = do
  (eof >>= return . const True)
  <||>
  (return False)

-- | Entrypoint for parsing a G4 regex. This does not get called
--   by the spliced parser, and is here for posterity (and debugging /
--   backwards compatibility).
parseRegex :: String -> Either ParseError (Regex Char)
parseRegex input = PP.parse (regexP rEOF') "" input

type RegexC = Regex Char

-- convert list of sequential regexes into a single regex
list2regex []  = Epsilon
list2regex [x] = x
list2regex xs  = Concat xs

-- | G4 regex parser, as used exclusively by the boot parser.
--
--   rEOF is a parser to indicate when it's okay to stop parsing the regex -}
regexP :: PS.Parser Bool -> PS.Parser RegexC
regexP rEOF = let

  regexP' :: PS.Parser [RegexC]
  regexP' = do
    traceM $ "regexP0"
    r <- extendedRegex regexElement
    traceM $ "regexP: " ++ show r
    whiteSpace
    b <- try rEOF
    traceM $ "regexP2: " ++ show b
    y <- getInput
    traceM $ "regexP3: " ++ show y
    if b
      then return [r]
      else do
        rs <- regexP'
        return $ r:rs
    <?> "regexP"

  in do
    xs <- regexP'
    return $ list2regex xs

extendedRegex foo = do
  whiteSpace
  negation <- optionMaybe (symbol "~")
  whiteSpace
  r <- foo
  whiteSpace
  p <- optionMaybe (try $ satisfy (`elem` "+*?"))
  whiteSpace
  let fncn = (case negation of
                (Just _) -> Negation
                _        -> id)
  return $ fncn (case p of
    Nothing  -> r
    Just '+' -> PosClos  r
    Just '*' -> Kleene   r
    Just '?' -> Question r
    Just _   -> undefined)


regexElement :: PS.Parser RegexC
regexElement = do
  whiteSpace
  r <- extendedRegex (charSet <||> literal <||> concatR <||> namedR <||> parens unionR)
  y <- getInput
  traceM $ show y
  return r

{-
  return (case rs of
    [] -> r
    _  -> Concat (r:rs))
-}

many2 p = do { x <- p; xs <- many p; return (x:xs) }

-- Named regex (upper-case identifier)
namedR :: PS.Parser RegexC
namedR = do
  s <- identifier
  return $ Named s

unionR :: PS.Parser RegexC
unionR = do
  traceM "<unionR>"
  u <- sepBy1 regexElement (traceM "OR" >> whiteSpace >> symbol "|" >> traceM "OR2")
  traceM "</unionR>"
  return $ Union u

concatR :: PS.Parser RegexC
concatR = do
  traceM "<concatR>"
  c <- many1 (extendedRegex (charSet <||> literal <||> parens regexElement)) >>= return . Concat
  traceM "</concatR>"
  return c

parseEscape :: String -> String
parseEscape s = (read $ "\"" ++ s ++ "\"") :: String

-- regex string literal uses single quotes
literal :: PS.Parser RegexC
literal = do
  r <- PC.between (char '\'') (char '\'') (many singleChar >>= (return . Literal . parseEscape))
  y <- getInput
  traceM $ "literal: " ++ show y
  return r

charSet :: PS.Parser RegexC
charSet = do
  traceM "<charSet>"
  whiteSpace
  cset <- PC.between (char '[') (char ']') (charSetBody >>= (return . CharSet . parseEscape))
  whiteSpace
  traceM $ "</charSet>: " ++ show cset
  return cset

charSetBody :: PS.Parser [Char]
charSetBody = do
  traceM $ "<charSetBody>"
  xs <- many $ charSetRange <||> (singleChar >>= (\c -> return [c]))
  traceM $ "charSetBody: " ++ (show $ concat xs)
  return $ concat xs

charSetRange :: PS.Parser [Char]
charSetRange = do
  start <- singleChar
  char '-'
  end   <- singleChar
  if ord end <= ord start
    then unexpected [end]
    else return [start..end]

singleChar =
  escapedChar
  <||>
  satisfy (\c -> not (c `elem` ['\'', ']']))

escapedChar :: PS.Parser Char
escapedChar = (do
    char '\\'
    char '\''
    return '\'')
  <||> (do
    char '\\'
    char ']'
    return ']')
  <||> (do
    char '\\'
    char '['
    return '[')
  <||> (do
    char '\\'
    char '\\'
    return '\\')

regexLexer :: PT.TokenParser ()
regexLexer = PT.makeTokenParser $ haskellStyle
  { reservedOpNames = ["[", "]", "//", "-", "+"] }

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
symbol        = PT.symbol      regexLexer

