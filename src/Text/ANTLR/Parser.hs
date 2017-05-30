{-# LANGUAGE DeriveGeneric, DeriveAnyClass, FlexibleContexts, InstanceSigs
           , UndecidableInstances, StandaloneDeriving, TypeFamilies
           , ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses #-}
module Text.ANTLR.Parser where
import Text.ANTLR.Allstar.Grammar
import Text.ANTLR.Pretty
import Text.ANTLR.Set (Generic(..))
import Text.ANTLR.Lex.Tokenizer (Token(..))

-- Action function is given the nonterminal we just matched on, and the
-- corresponding list of production elements (grammar symbols) in the RHS of the matched production
-- alternative, and the result of recursively.
-- *This* is (one of?) the data type that ties together terminal (token) types
-- and terminal symbol types. When the parser produces a terminal event, you're
-- seeing a *token*, but when the parser produces a nonterminal event, you're
-- seeing a production in the grammar firing which contains terminal *symbols*,
-- not tokens.
data ParseEvent ast nts t =
    TermE t
  | NonTE (nts, ProdElems nts (StripEOF (Sym t)), [ast])
  | EpsE

deriving instance (Show ast, Show nts, Show (StripEOF (Sym t)), Show t) => Show (ParseEvent ast nts t)

instance (Prettify ast, Prettify nts, Prettify (StripEOF (Sym t)), Prettify t) => Prettify (ParseEvent ast nts t) where
  prettify e = do
    pStr "Terminal Event: "
    incrIndent 2
    prettify e
    incrIndent (-2)


type Action ast nts t = ParseEvent ast nts t -> ast

-- An Icon (as used in first and follow sets of the LL1 parser and the
-- shift-reduce table of the LR1 parser) is just a terminal symbol taken from
-- the grammar, or it's an epsilon or EOF.
data Icon ts =
    Icon ts
  | IconEps
  | IconEOF -- End of input really, but EOF is ubiquitous.
  deriving (Generic, Hashable, Show, Eq, Ord)

-- *This* is the function defining the (n == Sym t == ts) relationship between
-- the *name* type of a token, the *symbol* type of a terminal token (as
-- constructed by the tokenizer), and the *terminal symbol* type as used by the
-- parser. When a parser wants to compare the symbol of an input token to a
-- terminal symbol found in the grammar, it should convert the token to an icon
-- using this function and then compare icons using Eq because icons throw away
-- the value of a token, leaving only the Eq-able piece that we care about.
token2symbol :: Token n v -> TokenSymbol n
token2symbol (Token n v) = TokenSymbol n
token2symbol EOF = EOFSymbol
token2symbol (Error s) = EOFSymbol

-- Tokens are symbolized by an icon containing their name.
instance Ref (Token n v) where
  type Sym (Token n v) = TokenSymbol n
  getSymbol = token2symbol

data TokenSymbol n =
    TokenSymbol n
  | EOFSymbol
  deriving (Eq, Ord, Show, Hashable, Generic)

-- A data type with an EOF constructor. There are two things you can do with a
-- data type that has an EOF:
-- - Ask for the type *without* the EOF at compile time
-- - Ask whether or not an instance is the EOF symbol at runtime
class HasEOF t where
  type StripEOF t :: * -- The unwrapped type (without the EOF data constructor alternative)
  isEOF :: t -> Bool -- Whether or not the given value of type t is the EOF value
  stripEOF :: t -> Maybe (StripEOF t)

instance HasEOF (TokenSymbol n) where
  type StripEOF (TokenSymbol n) = n

  isEOF EOFSymbol = True
  isEOF     _     = False

  stripEOF EOFSymbol       = Nothing
  stripEOF (TokenSymbol n) = Just n

instance HasEOF String where
  type StripEOF String = String
  
  isEOF "" = True
  isEOF _  = False

  stripEOF "" = Nothing
  stripEOF x  = Just x

-- 
--instance Ref (Token n v) => Stream (Token n v) where
--  type EOF n = Icon n
--  isEOF IconEOF = True
--  isEOF _       = False

instance (Prettify ts) => Prettify (Icon ts) where
  prettify IconEps  = pStr "iϵ"
  prettify IconEOF  = pStr "iEOF"
  prettify (Icon ts) = do
    pStr "i "
    prettify ts

isIcon Icon{} = True
isIcon _ = False

isIconEps IconEps = True
isIconEps _    = False

isIconEOF IconEOF = True
isIconEOF _   = False

-- Universal Abstract Syntax Tree data type. All internal AST "nodes" have a
-- nonterminal, the grammar production symbols it reduced from, and the
-- resulting recursively defined AST nodes acquired from the parser. Leaf AST
-- nodes can be either an epsilon (when explicit epsilons are used in the
-- grammar) or more importantly a terminal symbol.
-- *This* is another type that defines the relationship between the terminal
-- token type (t) and the terminal symbol type (ts == Sym t) where the AST tells
-- you the production rule that fired containing 'ts' as well as the tokens 't'
-- contained in leaves of the AST.
data AST nts t =
    LeafEps
  | Leaf t
  | AST nts (ProdElems nts (StripEOF (Sym t))) [AST nts t]
  deriving (Generic)

deriving instance (Eq (StripEOF (Sym t)), Eq nts, Eq t) => Eq (AST nts t)
deriving instance (Ord (StripEOF (Sym t)), Ord nts, Ord t) => Ord (AST nts t)
deriving instance (Show (StripEOF (Sym t)), Show nts, Show t) => Show (AST nts t)

instance (Prettify nts, Prettify t) => Prettify (AST nts t) where
  prettify LeafEps  = pStr "ϵ"
  prettify (Leaf t) = prettify t
  prettify (AST nts ps asts) = do
    prettify nts
    pStr "{"
    prettify asts
    pStr "}"

event2ast :: ParseEvent (AST nts t) nts t -> AST nts t
event2ast (TermE t)               = Leaf t
event2ast (NonTE (nts, ss, asts)) = AST nts ss asts

