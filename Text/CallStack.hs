module Text.ANTLR4.CallStack where

import Data.Graph

data CallStack = WildCard 
               | NonWildCard Graph
    deriving (Ord, Show, Eq)

isWildCard :: CallStack -> Bool
isWildCard WildCard = True
isWildCard _        = False

pop :: CallStack -> (State, CallStack)
pop = undefined

push :: State -> CallStack -> CallStack
push = undefined
