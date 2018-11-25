{-# LANGUAGE QuasiQuotes, TemplateHaskell, ScopedTypeVariables,
    OverloadedStrings #-}
{-|
  Module      : Language.ANTLR4.FileOpener
  Description : Quasiquoter for reading files by name at compile time
  Copyright   : (c) Karl Cronburg, 2018
  License     : BSD3
  Maintainer  : karl@cs.tufts.edu
  Stability   : experimental
  Portability : POSIX

  Just do the following. It'll make sense:

  @
    foo = id
    file_contents = [open| test/file.foo |]
  @
-}
module Language.ANTLR4.FileOpener (
  -- * File opening quasiquoter
    open
  ) where
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax (Exp(..), addDependentFile)
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Data.Text (strip, splitOn, pack, unpack)

-- | A quasiquoter for opening a file on disk, reading its contents, and running
--   a function by the same name as the file extension. e.g.:
--
-- @
--   foo = id
--   file_contents = [open| test/file.foo |]
-- @
--
-- @foo@ gets called on the contents of files with the extension @.foo@.
open :: QuasiQuoter
open = QuasiQuoter
  { quoteExp  = openExp
  , quotePat  = error "parse pattern"
  , quoteType = error "parse type"
  , quoteDec  = error "parse decl?"
  }

-- | Reads a file and runs a function with the name of the file extension,
--   returning the result for use by a quasiquoter.
openExp :: String -> TH.Q TH.Exp
openExp s = let
    fn  = unpack $ strip $ pack s
    ext = unpack $ last $ splitOn "." $ pack fn
  in do
    file_contents <- TH.runIO (readFile fn)
    addDependentFile fn
    [| $(return $ TH.VarE $ TH.mkName ext) file_contents |]

