{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Language.ANTLR4.Quote
( antlr4
) where

import Prelude hiding (exp, init)
import System.IO.Unsafe (unsafePerformIO)

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax (lift, Exp(..))
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Control.Monad (mapM)
import qualified Language.ANTLR4.Parser as NP
import qualified Text.Allstar.Grammar as AG

antlr4 :: QuasiQuoter
antlr4 =  QuasiQuoter aparse
                      (error "parse pattern")
                      (error "parse type")
                      (error "parse expression")


--   parser in quasiquotation monad
aparse :: String -> TH.Q TH.Exp
aparse input = do
 -- TODO: replace bad error showing with
 --       debugging information (filename, line #, column) in parser
 loc <- TH.location
 let fileName = TH.loc_filename loc
 let (line,column) = TH.loc_start loc

 case NP.parseANTLR fileName line column input of
   Left err -> unsafePerformIO $ fail $ show err
   Right x  -> g4_exp x

g4_exp :: [NP.G4] -> TH.Q TH.Exp -- exp :: G4
g4_exp ast = do
  asts <- mapM lift ast
  [| $(return $ ListE asts) |]

-- TODO convern G4 representation to Allstar Grammar representation
toAllstarGrammar :: [NP.G4] -> AG.Grammar s
toAllstarGrammar _ = AG.defaultGrammar
