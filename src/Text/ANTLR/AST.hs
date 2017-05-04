{-# LANGUAGE DeriveGeneric #-}
module Text.ANTLR.AST where
import Text.ANTLR.Allstar.Grammar (ProdElems(..))
import Text.ANTLR.Pretty
import Text.ANTLR.Set (Generic(..))

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

instance (Show nt, Show t) => Show (AST nt t) where
  show LeafEps  = "ϵ"
  show (Leaf t) = show t
  show (AST nt ps asts) = show nt ++ "{" ++ show asts ++ "}"

