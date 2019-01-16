{-# LANGUAGE DeriveLift, DeriveAnyClass, DeriveGeneric, OverloadedStrings #-}
{-|
  Module      : Language.ANTLR4.Boot.Syntax
  Description : Both the boot and core syntax data types for G4
  Copyright   : (c) Karl Cronburg, 2018
  License     : BSD3
  Maintainer  : karl@cs.tufts.edu
  Stability   : experimental
  Portability : POSIX
-}
module Language.ANTLR4.Boot.Syntax
  ( G4(..), PRHS(..), ProdElem(..), GAnnot(..)
  , Directive(..)
  , LRHS(..), Regex(..), isGTerm, isGNonTerm
  , TermAnnot(..), isMaybeAnnot, isNoAnnot, annot
  , prodElemSymbol
  ) where
import Text.ANTLR.Grammar ()
import Language.Haskell.TH.Lift (Lift(..))

import Language.Haskell.TH.Syntax (Exp)
import qualified Language.Haskell.TH.Syntax as S

import Text.ANTLR.Set ( Hashable(..), Generic(..) )
import Text.ANTLR.Pretty -- (rshow, Prettify(..), pStr, pshow, pStr')

-- | .g4 style syntax representation
data G4 = -- | Grammar name declaration in g4
          Grammar { gName :: String -- ^ Name
                  }
        -- | One or more g4 productions
        | Prod { pName    :: String -- ^ Production's name
               , patterns :: [PRHS] -- ^ List of rules to match on
               }
        -- | A single, possibly annotated, g4 lexical rule
        | Lex  { annotation :: Maybe GAnnot -- ^ Lexical annotation (@fragment@)
               , lName      :: String       -- ^ Lexical rule name
               , pattern    :: LRHS         -- ^ The regex to match on
               }
  deriving (Show, Eq, Lift, Generic, Hashable)

instance Prettify G4 where
  prettify (Grammar gn) = do
    pStr "grammar "
    pStr' gn
  prettify (Prod n ps) = do
    pStr' n
    pStr " -> "
    incrIndent $ length n + 4
    pListLines ps
    incrIndent $ 0 - (length n + 4)
  prettify (Lex annot ln (LRHS regex dir)) = do
    pStr' ln
    pStr " -> "
    incrIndent $ length ln + 4
    prettify regex
    incrIndent $ 0 - (length ln + 4)
    pStr "("
    prettify dir
    pStr ")"


instance Lift Exp

-- | The right-hand side of a G4 production rule.
data PRHS = PRHS
  { alphas      :: [ProdElem] -- ^ In-order list of elements defining this rule
  , pred        :: Maybe Exp  -- ^ Arbitrary boolean predicate to test whether or not this rule should fire
  , mutator     :: Maybe Exp  -- ^ Arbitrary mutator to run when this rule fires
  , pDirective  :: Maybe Directive -- ^ How to construct a Haskell type when this rules fires
  } deriving (Show, Eq, Lift, Generic)

instance Prettify PRHS where
  prettify (PRHS as pred mut pDir) = do
    prettify as
    pStr "("
    pStr' $ show pred; pStr ","
    pStr' $ show mut;  pStr ","
    prettify pDir; pStr ")"

-- | Antiquoted (or g4-embedded) string that goes to the right of an arrow in
--   a g4 production rule. This specifies how to construct a Haskell type.
data Directive =
    UpperD String   -- ^ Probably a Haskell data constructor
  | LowerD String   -- ^ Probably just a Haskell function to call
  | HaskellD String -- ^ Arbitrary antiquoted Haskell code embedded in the G4 grammar
  deriving (Show, Eq, Ord, Lift, Generic, Hashable)

instance Prettify Directive where prettify = rshow

instance Hashable PRHS where
  hashWithSalt salt prhs = salt `hashWithSalt` alphas prhs

-- | Annotations on a term (nonterminal or terminal) for extending our G4
--   BNF-like syntax with regular expression modifiers.
data TermAnnot =
    Regular Char -- ^ Regular expression modifier (e.g. +, ?, *)
  | NoAnnot      -- ^ Term is not annotated with anything
  deriving (Show, Eq, Ord, Lift, Generic, Hashable)

instance Prettify TermAnnot where
  prettify NoAnnot = return ()
  prettify (Regular c) = pStr' [c]

-- | Get the annotation from a 'ProdElem'
annot :: ProdElem -> TermAnnot
annot (GTerm a _) = a
annot (GNonTerm a _) = a

-- | Is this 'TermAnnot' a maybe?
isMaybeAnnot :: TermAnnot -> Bool
isMaybeAnnot (Regular '?') = True
isMaybeAnnot _             = False

-- | Does this 'TermAnnot' have no annotation?
isNoAnnot :: TermAnnot -> Bool
isNoAnnot NoAnnot = True
isNoAnnot _       = False

-- | A single production element with any accompanying regex annotation
data ProdElem =
    GTerm     TermAnnot String -- ^ G4 terminal
  | GNonTerm  TermAnnot String -- ^ G4 nonterminal
  deriving (Show, Eq, Ord, Lift, Generic, Hashable)

instance Prettify ProdElem where
  prettify (GTerm annot s) = do
    pStr' s
    prettify annot
  prettify (GNonTerm annot s) = do
    pStr' s
    prettify annot

prodElemSymbol (GTerm _ s) = s
prodElemSymbol (GNonTerm _ s) = s

-- | Is this a terminal G4 element?
isGTerm (GTerm _ _) = True
isGTerm _           = False

-- | Is this a nonterminal G4 element?
isGNonTerm (GNonTerm _ _) = True
isGNonTerm _              = False

-- | Allowable annotations on a lexical production rule
data    GAnnot   = Fragment -- ^ For now the only annotation is @fragment@.
  deriving (Show, Eq, Lift, Generic, Hashable)

-- | Right-hand side of a lexical G4 rule
data LRHS = LRHS
  { regex     :: Regex Char      -- ^ A regular expression over characters as tokens.
  , directive :: Maybe Directive -- ^ Optional directive: @Nothing@ is equivalent to @(Just "String")@.
  }
  deriving (Show, Eq, Lift, Generic, Hashable)

-- | G4 representation of a regex (G4 regex syntax, not regexs used by tokenizer)
data Regex s =
    Epsilon              -- ^ Consume no input
  | Literal    [s]       -- ^ Match on a literal string (sequence of characters)
  | Union      [Regex s] -- ^ Match on any
  | Concat     [Regex s] -- ^ Match in sequence
  | Kleene     (Regex s) -- ^ Match zero or more times
  | PosClos    (Regex s) -- ^ Match one or more times
  | Question   (Regex s) -- ^ Match zero or one time.
  | CharSet    [s]       -- ^ Match once on any of the characters
  | Negation   (Regex s) -- ^ Match anything that doesn't match this
  | Named      String    -- ^ A reference to some other regex (need to track an environment)
  deriving (Lift, Eq, Show, Generic, Hashable)
-- TODO: Lex regexs (e.g. complement sets, escape chars, ...)
-- TODO: Set s, and ranges of characters

instance (Show s, Prettify s) => Prettify (Regex s) where
  prettify Epsilon = pStr "ε"
  prettify (Literal ss) = do
    pStr "\""
    mapM prettify ss
    pStr "\""
  prettify rest = pStr' $ show rest

