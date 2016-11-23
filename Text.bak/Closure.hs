module Text.ANTLR4.Closure where

import Text.ANTLR4.State
import Text.ANTLR4.CallStack
import Data.Set

type Closure = (State, Int, CallStack)
