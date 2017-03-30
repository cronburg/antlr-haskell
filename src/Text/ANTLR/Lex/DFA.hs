module Text.ANTLR.Lex.DFA where
import Text.ANTLR.Lex.Automata

type Edge s = s

type State i = i

type DFA s i = Automata (Edge s) s (State i)

