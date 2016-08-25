module Text.ANTLR4.State where

import Text.ANTLR4.Grammar  (NonTerminal)
import Text.ANTLR4.ATN      (Transition)

data State = Start  (NonTerminal, [Transition])
           | Branch (NonTerminal, [Transition])
           | Final  NonTerminal
    deriving (Ord, Show, Eq)

isStop :: State -> Bool
isStop (Final {}) = True
isStop  _        = False
