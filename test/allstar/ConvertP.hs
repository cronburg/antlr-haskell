{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, QuasiQuotes
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, TemplateHaskell, FlexibleContexts #-}
module ConvertP
  ( module ConvertDFA
  , module ConvertP
  ) where
import Language.ANTLR4
import ConvertDFA

$(g4_parsers convertAST convertGrammar)

