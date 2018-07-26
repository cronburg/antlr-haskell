module Text.ANTLR.Common where

concatWith cs [] = []
concatWith cs [x] = x
concatWith cs (x:xs) = x ++ cs ++ concatWith cs xs

