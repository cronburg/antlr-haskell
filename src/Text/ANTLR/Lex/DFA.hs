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

-- | DFA edges are just the symbols of our alphabet.
type Edge s = s

-- | DFA states are just some Eq-able value, likely integers @i@
type State i = i

-- | A DFA is an automata with edges labeled by symbols @s@ and nodes representing
--   states labeled by some type @i@.
type DFA s i = Automata (Edge s) s (State i)

