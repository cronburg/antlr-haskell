{-# LANGUAGE DeriveLift #-}
module Language.ANTLR4.Syntax
  ( G4(..), PRHS(..), GTerm(..), GNonTerm(..), GAnnot(..)
  , LRHS(..), Regex(..)
  ) where
import Text.ANTLR.Allstar.Grammar ()
import Language.Haskell.TH.Lift (Lift(..))

import Language.ANTLR4.Regex (Regex(..))

--import Language.Haskell.TH       
import Language.Haskell.TH.Syntax (Exp)

-- .g4 style syntax
data G4 = Grammar {gName :: String}
        | Prod {pName :: String, patterns :: [PRHS] }
        | Lex  {annotation :: Maybe GAnnot, lName :: String, pattern :: LRHS }
  deriving (Show, Eq, Lift)

instance Lift Exp

data PRHS     = PRHS { alphas :: [Either GTerm GNonTerm]
                     , pred :: Maybe Exp
                     , mutator :: Maybe Exp
                     }
  deriving (Show, Eq, Lift)

newtype GTerm    = GTerm String
  deriving (Show, Eq, Ord, Lift)
newtype GNonTerm = GNonTerm String
  deriving (Show, Eq, Ord, Lift)
data    GAnnot   = Fragment
  deriving (Show, Eq, Lift)

data LRHS     = LRHS { regex     :: Regex Char
                     , directive :: Maybe String
                     }
  deriving (Show, Eq, Lift)

