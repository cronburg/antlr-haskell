module Text.ANTLR.Parser where
import Text.ANTLR.Allstar.Grammar

-- Action function is given the nonterminal we just matched on, and the
-- corresponding list of production elements (grammar symbols) in the RHS of the matched production
-- alternative, and the result of recursively
data ParseEvent ast nt t =
    TermE (Icon t)
  | NonTE (nt, ProdElems nt t, [ast])
  | EpsE

type Action ast nt t = ParseEvent ast nt t -> ast

-- An LL1 Icon (as used in first and follow sets) is either a
-- terminal in the grammar's alphabet (with an associated *symbol*), or an
-- epsilon icon, or the end-of-file icon.
data Icon t =
    Icon t
  | IconEps
  | IconEOF -- End of input really, but EOF is ubiquitous.
  deriving (Ord, Show)

instance (Referent t) => Eq (Icon t) where
  Icon t0 == Icon t1 = getSymbol t0 == getSymbol t1
  IconEps == IconEps = True
  IconEOF == IconEOF = True
  _ == _ = False

isIcon Icon{} = True
isIcon _ = False

isIconEps IconEps = True
isIconEps _    = False

isIconEOF IconEOF = True
isIconEOF _   = False

