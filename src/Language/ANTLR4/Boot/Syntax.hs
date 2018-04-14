{-# LANGUAGE DeriveLift, DeriveAnyClass, DeriveGeneric #-}
module Language.ANTLR4.Boot.Syntax
  ( G4(..), PRHS(..), ProdElem(..), GAnnot(..)
  , LRHS(..), Regex(..), isGTerm, isGNonTerm
  , TermAnnot(..), isMaybeAnnot, isNoAnnot, annot
  ) where
import Text.ANTLR.Grammar ()
import Language.Haskell.TH.Lift (Lift(..))

import Language.ANTLR4.Regex (Regex(..))

--import Language.Haskell.TH       
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
  , pDirective  :: Maybe String
  } deriving (Show, Eq, Lift, Generic)

instance Hashable PRHS where
  hashWithSalt salt prhs = salt `hashWithSalt` alphas prhs

data TermAnnot =
    Regular Char  -- Regular expression modifier (e.g. +, ?, *)
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
                     , directive :: Maybe String
                     }
  deriving (Show, Eq, Lift, Generic, Hashable)

{-
instance Hashable Exp

instance Hashable S.Name
instance Hashable S.OccName
instance Hashable S.Lit
instance Hashable S.NameFlavour
instance Hashable S.Pat
instance Hashable S.Match
instance Hashable S.Type
instance Hashable S.ModName
instance Hashable S.Guard
instance Hashable S.TyVarBndr
instance Hashable S.Body
instance Hashable S.Dec
instance Hashable S.Stmt
instance Hashable S.TyLit
instance Hashable S.Range
instance Hashable S.Clause
instance Hashable S.NameSpace
instance Hashable S.Con
instance Hashable S.PkgName
instance Hashable S.FunDep
instance Hashable S.Bang
instance Hashable S.SourceUnpackedness
instance Hashable S.Overlap
instance Hashable S.SourceStrictness
instance Hashable S.Foreign
instance Hashable S.Callconv
instance Hashable S.Fixity
instance Hashable S.FixityDirection
instance Hashable S.Safety
instance Hashable S.Pragma
instance Hashable S.Inline
instance Hashable S.TySynEqn
instance Hashable S.TypeFamilyHead
instance Hashable S.RuleMatch
-}

