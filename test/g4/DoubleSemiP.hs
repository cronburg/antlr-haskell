{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, QuasiQuotes
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, TemplateHaskell, FlexibleContexts #-}
module DoubleSemiP
  ( module DoubleSemiP
  , module DoubleSemi
  ) where
import Language.ANTLR4
import DoubleSemi

$(g4_parsers dblAST dblGrammar)

