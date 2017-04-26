{-# LANGUAGE QuasiQuotes, TemplateHaskell, ScopedTypeVariables,
    OverloadedStrings #-}
module Language.ANTLR4.FileOpener
( open
) where
import Prelude hiding (exp, init)
import System.IO.Unsafe (unsafePerformIO)

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax (lift, Exp(..), addDependentFile)
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Data.Text (strip, splitOn, pack, unpack)

open :: QuasiQuoter
open = QuasiQuoter
  { quoteExp  = openExp
  , quotePat  = error "parse pattern"
  , quoteType = error "parse type"
  , quoteDec  = error "parse decl?"
  }

openExp :: String -> TH.Q TH.Exp
openExp s = let
    fn  = unpack $ strip $ pack s
    ext = unpack $ last $ splitOn "." $ pack fn
  in do
    file_contents <- TH.runIO (readFile fn)
    addDependentFile fn
    [| $(return $ TH.VarE $ TH.mkName ext) file_contents |]

