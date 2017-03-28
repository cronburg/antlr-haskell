module Text.ANTLR.AST where
import Text.ANTLR.Allstar.Grammar (ProdElems(..))

-- Universal Abstract Syntax Tree data type. All internal AST "nodes" have a
-- nonterminal, the grammar production symbols it reduced from, and the
-- resulting recursively defined AST nodes acquired from the parser. Leaf AST
-- nodes can be either an epsilon (when explicit epsilons are used in the
-- grammar) or more importantly a terminal symbol.
data AST nt t =
    LeafEps
  | Leaf t
  | AST nt (ProdElems nt t) [AST nt t]
  deriving (Eq, Ord, Show)

