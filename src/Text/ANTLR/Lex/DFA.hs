{-|
  Module      : Text.ANTLR.Lex.DFA
  Description : Deterministic finite automaton types
  Copyright   : (c) Karl Cronburg, 2018
  License     : BSD3
  Maintainer  : karl@cs.tufts.edu
  Stability   : experimental
  Portability : POSIX

-}
module Text.ANTLR.Lex.DFA where
import Text.ANTLR.Lex.Automata

type Edge s = s

type State i = i

type DFA s i = Automata (Edge s) s (State i)

