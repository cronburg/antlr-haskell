module Text.ANTLR.Lex.DFA where
import Text.ANTLR.Lex.Automata

type Edge s = s

type DFA s i = Automata (Edge s) s i

