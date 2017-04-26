module Language.Chisel.Tokenizer where
import qualified Text.ANTLR.Lex.Tokenizer as T
import Text.ANTLR.Lex.Regex

data Name =
    T_LowerID
  | T_UpperID
  | T_Prim
  | T_INT
  | T_Arrow
  | T_LParen
  | T_RParen
  | T_Pound
  | T_VerticalBar
  | T_Colon
  | T_Comma
  | T_AtSymbol
  | T_LineComment

data Value =
    LowerID String
  | UpperID String
  | Prim    Primitive
  | INT     Int
  | Arrow
  | LParen
  | RParen
  | Pound
  | VerticalBar
  | Colon
  | Comma
  | AtSymbol
  | LineComment String

prims = ["page", "pages", "word", "words", "byte", "bytes", "bit", "bits"]

regexes =
  [ (T_LowerID,     Concat [Class ['a' .. 'z'], Class $ '_' : ['a' .. 'z'] ++ ['A' .. 'Z']])
  , (T_UpperID,     Concat [Class ['A' .. 'Z'], Class $ '_' : ['a' .. 'z'] ++ ['A' .. 'Z']])
  , (T_Prim,        MultiUnion $ map Literal prims)
  , (T_INT,         Class ['0' .. '9'])
  , (T_Arrow,       Literal "->")
  , (T_LParen,      Symbol '(')
  , (T_RParen,      Symbol ')')
  , (T_Pound,       Symbol '#')
  , (T_VerticalBar, Symbol '|')
  , (T_Colon,       Symbol ':')
  , (T_Comma,       Symbol ',')
  , (T_AtSymbol,    Symbol '@')
  , (T_LineComment, Concat [Literal "//", Kleene $ NotClass ['\n'], Symbol '\n'])
  ]

dfas = map (regex2dfa . snd) regexs

data Primitive = Page | Word | Byte | Bit

prim "page"  = Page
prim "pages" = Page
prim "word"  = Word
prim "words" = Word
prim "byte"  = Byte
prim "bytes" = Byte
prim "bit"   = Bit
prim "bits"  = Bit

lexeme2value l n = case n of
  T_LowerID     -> LowerID l
  T_UpperID     -> UpperID l
  T_Prim        -> Prim $ prim l
  T_INT         -> INT  $ read l
  T_Arrow       -> Arrow
  T_LParen      -> LParen
  T_RParen      -> RParen
  T_Pound       -> Pound
  T_VerticalBar -> VerticalBar
  T_Colon       -> Colon
  T_Comma       -> Comma
  T_AtSymbol    -> AtSymbol
  T_LineComment -> LineComment l

tokenize :: String -> [T.Token Name Value]
tokenize = T.tokenize dfas

