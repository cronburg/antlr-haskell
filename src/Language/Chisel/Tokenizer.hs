{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
module Language.Chisel.Tokenizer
  ( Name(..), Value(..), Primitive(..)
  , tokenize
  , lowerID, upperID, prim, int, arrow, lparen, rparen, pound, vertbar, colon
  , comma, atsymbol, carrot, dot, linecomm, ws, isWhitespace
  ) where
import qualified Text.ANTLR.Lex.Tokenizer as T
import Text.ANTLR.Lex.Regex
import Control.Arrow ( (&&&) )
import Text.ANTLR.Set (Hashable(..), Generic(..))
import Text.ANTLR.Pretty

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
  | T_Carrot
  | T_Dot
  | T_LineComment
  | T_WS
  deriving (Eq, Ord, Enum, Show, Bounded, Hashable, Generic, Prettify)

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
  | Carrot
  | Dot
  | LineComment String
  | WS          String
  deriving (Show, Ord, Eq, Generic, Hashable, Prettify)

lowerID x = T.Token T_LowerID $   LowerID x
upperID x = T.Token T_UpperID $   UpperID x
prim    x = T.Token T_Prim    $   Prim x
int     x = T.Token T_INT     $   INT x
arrow     = T.Token T_Arrow       Arrow
lparen    = T.Token T_LParen      LParen
rparen    = T.Token T_RParen      RParen
pound     = T.Token T_Pound       Pound
vertbar   = T.Token T_VerticalBar VerticalBar
colon     = T.Token T_Colon       Colon
comma     = T.Token T_Comma       Comma
atsymbol  = T.Token T_AtSymbol    AtSymbol
carrot    = T.Token T_Carrot      Carrot
dot       = T.Token T_Dot         Dot
linecomm x = T.Token T_LineComment $ LineComment x
ws       x = T.Token T_WS          $ WS x

-- TODO: Goes in a Token type class
isWhitespace (T.Token T_LineComment _) = True
isWhitespace (T.Token T_WS _) = True
isWhitespace _ = False

prims = ["page", "pages", "word", "words", "byte", "bytes", "bit", "bits"]

idCharacters = Kleene $ Class $ '_' : ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']

regexes =
  [ (T_Prim,        MultiUnion $ map Literal prims)
  , (T_LowerID,     Concat [Class ['a' .. 'z'], idCharacters])
  , (T_UpperID,     Concat [Class ['A' .. 'Z'], idCharacters])
  , (T_INT,         Class ['0' .. '9'])
  , (T_Arrow,       Literal "->")
  , (T_LParen,      Symbol '(')
  , (T_RParen,      Symbol ')')
  , (T_Pound,       Symbol '#')
  , (T_VerticalBar, Symbol '|')
  , (T_Colon,       Symbol ':')
  , (T_Comma,       Symbol ',')
  , (T_AtSymbol,    Symbol '@')
  , (T_Carrot,      Symbol '^')
  , (T_Dot,         Symbol '.')
  , (T_LineComment, Concat [Literal "//", Kleene $ NotClass ['\n'], Symbol '\n'])
  , (T_WS,          Kleene $ Class " \t\n\r\f\v")
  ]

dfas = map (fst &&& regex2dfa . snd) regexes

-- Todo: ANTLR should code-gen this as a pattern match on the set of possible
-- DFAs? Or hang name attributes on the DFAs to make the lookup O(1) (in case
-- this ever becomes a bottleneck in the tokenizer)
dfaGetName dfa = case filter ((== dfa) . snd) dfas of
  []            -> undefined -- Unknown DFA given
  ((name,_):[]) -> name
  _             -> undefined -- Ambiguous (identical) DFAs found during tokenization

data Primitive = Page | Word | Byte | Bit
  deriving (Show, Eq, Ord, Generic, Hashable, Prettify)

lexeme2prim "page"  = Page
lexeme2prim "pages" = Page
lexeme2prim "word"  = Word
lexeme2prim "words" = Word
lexeme2prim "byte"  = Byte
lexeme2prim "bytes" = Byte
lexeme2prim "bit"   = Bit
lexeme2prim "bits"  = Bit

lexeme2value l n = case n of
  T_LowerID     -> LowerID l
  T_UpperID     -> UpperID l
  T_Prim        -> Prim $ lexeme2prim l
  T_INT         -> INT  $ read l
  T_Arrow       -> Arrow
  T_LParen      -> LParen
  T_RParen      -> RParen
  T_Pound       -> Pound
  T_VerticalBar -> VerticalBar
  T_Colon       -> Colon
  T_Comma       -> Comma
  T_AtSymbol    -> AtSymbol
  T_Carrot      -> Carrot
  T_Dot         -> Dot
  T_LineComment -> LineComment l
  T_WS          -> WS l

tokenize :: String -> [T.Token Name Value]
tokenize = T.tokenize (map snd dfas) dfaGetName lexeme2value

