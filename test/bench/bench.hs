{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, QuasiQuotes
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, TemplateHaskell #-}
module Main where
import Language.ANTLR4
import Grammar

isWS T_WS = True
isWS _ = False

main :: IO ()
main = print $ glrParse isWS "a"
