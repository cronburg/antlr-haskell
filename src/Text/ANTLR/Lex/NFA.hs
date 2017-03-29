module Text.ANTLR.Lex.NFA where
import Text.ANTLR.Lex.Automata

data Edge s = Edge s | NFAEpsilon
  deriving (Ord, Eq)

type NFA s i = Automata (Edge s) s i

