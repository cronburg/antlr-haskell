{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, QuasiQuotes
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, TemplateHaskell, FlexibleContexts #-}
module EmptyP
  ( module Empty
  , module EmptyP
  ) where
import Language.ANTLR4
import Empty

$(g4_parsers emptyAST emptyGrammar)

