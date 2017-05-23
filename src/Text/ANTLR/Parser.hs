{-# LANGUAGE DeriveGeneric, DeriveAnyClass, FlexibleContexts, InstanceSigs
           , UndecidableInstances, StandaloneDeriving #-}
module Text.ANTLR.Parser where
import Text.ANTLR.Allstar.Grammar
import Text.ANTLR.Pretty
import Text.ANTLR.Set (Generic(..))

-- Action function is given the nonterminal we just matched on, and the
-- corresponding list of production elements (grammar symbols) in the RHS of the matched production
-- alternative, and the result of recursively
data ParseEvent ast nt t =
    TermE (Icon t)
  | NonTE (nt, ProdElems nt t, [ast])
  | EpsE
  deriving (Show)

type Action ast nt t = ParseEvent ast nt t -> ast

-- An LL1 Icon (as used in first and follow sets) is either a
-- terminal in the grammar's alphabet (with an associated *symbol*), or an
-- epsilon icon, or the end-of-file icon.
data Icon t =
    Icon t
  | IconEps
  | IconEOF -- End of input really, but EOF is ubiquitous.
  deriving (Hashable, Generic, Show)

instance (Prettify t) => Prettify (Icon t) where
  prettify IconEps  = pStr "iϵ"
  prettify IconEOF  = pStr "iEOF"
  prettify (Icon t) = do
    pStr "i "
    prettify t

-- Icon equivalence only cares about the symbol (not the terminal value attached
-- to the icon)
instance (Eq (Sym t), Ref t) => Eq (Icon t) where
  (==) :: (Eq (Sym t)) => Icon t -> Icon t -> Bool
  Icon t0 == Icon t1 = getSymbol t0 == getSymbol t1
  IconEps == IconEps = True
  IconEOF == IconEOF = True
  _ == _ = False

deriving instance (Ref t, Eq (Sym t), Ord t) => Ord (Icon t)

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
data AST nt t =
    LeafEps
  | Leaf t
  | AST nt (ProdElems nt t) [AST nt t]
  deriving (Eq, Ord, Generic)

instance (Prettify nt, Prettify t) => Prettify (AST nt t) where
  prettify LeafEps  = pStr "ϵ"
  prettify (Leaf t) = prettify t
  prettify (AST nt ps asts) = do
    prettify nt
    pStr "{"
    prettify asts
    pStr "}"

event2ast :: ParseEvent (AST nt t) nt t -> AST nt t
event2ast (TermE (Icon t))       = Leaf t
event2ast (TermE IconEps)        = LeafEps
event2ast (NonTE (nt, ss, asts)) = AST nt ss asts

