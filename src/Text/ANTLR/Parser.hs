{-# LANGUAGE DeriveGeneric, DeriveAnyClass, FlexibleContexts, InstanceSigs
           , UndecidableInstances, StandaloneDeriving, TypeFamilies
           , ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses
           , OverloadedStrings, DeriveDataTypeable #-}
{-|
  Module      : Text.ANTLR.Parser
  Description : Parsing API for constructing Haskell data types from lists of tokens
  Copyright   : (c) Karl Cronburg, 2018
  License     : BSD3
  Maintainer  : karl@cs.tufts.edu
  Stability   : experimental
  Portability : POSIX

-}
module Text.ANTLR.Parser where
import Text.ANTLR.Grammar hiding (Action)
import Text.ANTLR.Pretty
import Text.ANTLR.Set (Generic(..))
import Text.ANTLR.Lex.Tokenizer (Token(..))
import Data.Data (Data(..))
import Language.Haskell.TH.Lift (Lift(..))
import Text.ANTLR.Set (Hashable)

-- | Action functions triggered during parsing are given the nonterminal we just matched on, the
--   corresponding list of production elements (grammar symbols) in the RHS of the matched production
--   alternative, and the result of recursively.
--
--   A 'ParseEvent' may also be just a terminal matched on, or an epsilon event
--   based heavily on which parsing algorithm is being run.
--
--   __This__ data type is one of the data types that tie together terminal (token) types
--   and terminal symbol types. When the parser produces a terminal event, you're
--   seeing a __token__, but when the parser produces a nonterminal event, you're
--   seeing a production in the grammar firing which contains terminal __symbols__,
--   not tokens.
data ParseEvent ast nts t =
    TermE t -- ^ A terminal was seen in the input
  | NonTE (nts, ProdElems nts (StripEOF (Sym t)), [ast]) -- ^ A non-terminal was seen in the input
  | EpsE -- ^ Epsilon event

deriving instance (Show ast, Show nts, Show (StripEOF (Sym t)), Show t) => Show (ParseEvent ast nts t)

instance (Prettify ast, Prettify nts, Prettify (StripEOF (Sym t)), Prettify t) => Prettify (ParseEvent ast nts t) where
  prettify e = do
    pStr "Terminal Event: "
    incrIndent 2
    prettify e
    incrIndent (-2)

-- | An Action as seen by the host language (Haskell) is a function from parse
--   events to an abstract-syntax tree that the function constructs based on which
--   non-terminal or terminal symbol was seen.
type Action ast nts t = ParseEvent ast nts t -> ast

-- | An Icon (as used in first and follow sets of the LL1 parser and the
--   shift-reduce table of the LR1 parser) is just a terminal symbol taken from
--   the grammar, or it's an epsilon or EOF.
data Icon ts =
    Icon ts  -- ^ Terminal symbol icon
  | IconEps  -- ^ Epsilon icon
  | IconEOF  -- ^ EOF (end of file / input) icon
  deriving (Generic, Hashable, Show, Eq, Ord, Data, Lift)

-- | __This__ is the function defining the (n == Sym t == ts) relationship between
--   the __name__ type of a token, the __symbol__ type of a terminal token (as
--   constructed by the tokenizer), and the __terminal symbol__ type as used by the
--   parser. When a parser wants to compare the symbol of an input token to a
--   terminal symbol found in the grammar, it should convert the token to an icon
--   using this function and then compare icons using Eq because icons throw away
--   the value of a token, leaving only the Eq-able piece that we care about.
token2symbol :: Token n v -> TokenSymbol n
token2symbol (Token n v _) = TokenSymbol n
token2symbol EOF = EOFSymbol
token2symbol (Error s) = EOFSymbol

-- | Tokens are symbolized by an icon containing their name.
instance Ref (Token n v) where
  type Sym (Token n v) = TokenSymbol n
  getSymbol = token2symbol

-- | The symbol for some tokenize is either just it's name @n@ or the special EOF symbol.
data TokenSymbol n =
    TokenSymbol n  -- ^ Named symbol
  | EOFSymbol      -- ^ End-of-file symbol
  deriving (Eq, Ord, Show, Hashable, Generic)

-- | A data type with an EOF constructor. There are two things you can do with a
--   data type that has an EOF:
--
-- > Ask for the type *without* the EOF at compile time
-- > Ask whether or not an instance is the EOF symbol at runtime
--
class HasEOF t where
  -- | The unwrapped type (without the EOF data constructor alternative)
  type StripEOF t :: *
  -- | Whether or not the given value of type t is the EOF value
  isEOF :: t -> Bool
  -- | Take a token and try to unwrap its name (an EOF should result in Nothing)
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

instance (Prettify ts) => Prettify (Icon ts) where
  prettify IconEps  = pStr "iϵ"
  prettify IconEOF  = pStr "iEOF"
  prettify (Icon ts) = do
    pStr "Icon "
    prettify ts

-- | Is this a terminal-symbol icon?
isIcon Icon{} = True
isIcon _ = False

-- | Is this an epsilon icon?
isIconEps IconEps = True
isIconEps _    = False

-- | Is this the EOF icon?
isIconEOF IconEOF = True
isIconEOF _   = False

-- | Universal Abstract Syntax Tree data type. All internal AST "nodes" have a
--   nonterminal, the grammar production symbols it reduced from, and the
--   resulting recursively defined AST nodes acquired from the parser. Leaf AST
--   nodes can be either an epsilon (when explicit epsilons are used in the
--   grammar) or more importantly a terminal symbol.
--   __This__ is another type that defines the relationship between the terminal
--   token type @t@ and the terminal symbol type @(ts == Sym t)@ where the AST tells
--   you the production rule that fired containing @ts@ as well as the tokens @t@
--   contained in leaves of the AST.
data AST nts t =
    LeafEps -- ^ Epsilon leaf AST node
  | Leaf t  -- ^ Terminal token leaf in the AST
  | AST nts (ProdElems nts (StripEOF (Sym t))) [AST nts t] -- ^ Internal AST node
  deriving (Generic)

deriving instance (Eq (StripEOF (Sym t)), Eq nts, Eq t) => Eq (AST nts t)
deriving instance (Ord (StripEOF (Sym t)), Ord nts, Ord t) => Ord (AST nts t)
deriving instance (Show (StripEOF (Sym t)), Show nts, Show t) => Show (AST nts t)
deriving instance (Hashable (StripEOF (Sym t)), Hashable nts, Hashable t) => Hashable (AST nts t)

instance (Prettify nts, Prettify t) => Prettify (AST nts t) where
  prettify LeafEps  = pStr "ϵ"
  prettify (Leaf t) = prettify t
  prettify (AST nts ps asts) = do
    prettify nts
    pStr "{"
    prettify asts
    pStr "}"

-- | Default AST-constructor function which just copies over the contents of
--   some parse event into an 'AST'.
event2ast :: ParseEvent (AST nts t) nts t -> AST nts t
event2ast (TermE t)               = Leaf t
event2ast (NonTE (nts, ss, asts)) = AST nts ss asts

