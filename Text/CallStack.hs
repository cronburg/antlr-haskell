module Text.ANTLR4.CallStack where

import Data.Graph

data CallStack = WildCard 
               | NonWildCard Graph
