{-# LANGUAGE DeriveLift, DeriveAnyClass, DeriveGeneric #-}
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
  , LRHS(..), Regex(..), isGTerm, isGNonTerm
  , TermAnnot(..), isMaybeAnnot, isNoAnnot, annot
  , Directive(..)
  ) where
import Text.ANTLR.Grammar ()
import Language.Haskell.TH.Lift (Lift(..))

import Language.ANTLR4.Regex (Regex(..))

import Language.Haskell.TH.Syntax (Exp)
import qualified Language.Haskell.TH.Syntax as S

import Text.ANTLR.Set ( Hashable(..), Generic(..) )

-- .g4 style syntax
data G4 = Grammar {gName :: String}
        | Prod {pName :: String, patterns :: [PRHS] }
        | Lex  {annotation :: Maybe GAnnot, lName :: String, pattern :: LRHS }
  deriving (Show, Eq, Lift, Generic, Hashable)

instance Lift Exp

data PRHS = PRHS
  { alphas      :: [ProdElem]
  , pred        :: Maybe Exp
  , mutator     :: Maybe Exp
  , pDirective  :: Maybe Directive
  } deriving (Show, Eq, Lift, Generic)

data Directive =
    UpperD String
  | LowerD String
  | HaskellD String
  deriving (Show, Eq, Lift, Generic, Hashable)

instance Hashable PRHS where
  hashWithSalt salt prhs = salt `hashWithSalt` alphas prhs

data TermAnnot =
    Regular Char -- Regular expression modifier (e.g. +, ?, *)
  | NoAnnot
  deriving (Show, Eq, Ord, Lift, Generic, Hashable)

annot (GTerm a _) = a
annot (GNonTerm a _) = a

isMaybeAnnot (Regular '?') = True
isMaybeAnnot _             = False

isNoAnnot NoAnnot = True
isNoAnnot _       = False

data ProdElem =
    GTerm     TermAnnot String
  | GNonTerm  TermAnnot String
  deriving (Show, Eq, Ord, Lift, Generic, Hashable)

isGTerm (GTerm _ _) = True
isGTerm _           = False

isGNonTerm (GNonTerm _ _) = True
isGNonTerm _              = False

data    GAnnot   = Fragment
  deriving (Show, Eq, Lift, Generic, Hashable)

data LRHS     = LRHS { regex     :: Regex Char
                     , directive :: Maybe Directive
                     }
  deriving (Show, Eq, Lift, Generic, Hashable)

