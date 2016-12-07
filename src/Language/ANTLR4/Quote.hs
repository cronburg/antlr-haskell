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
import qualified Language.ANTLR4.Parser as G4P
import qualified Text.ANTLR.Allstar.Grammar as AG

import Data.Set (Set(..))
import qualified Data.Set as Set

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

 case G4P.parseANTLR fileName line column input of
   Left err -> unsafePerformIO $ fail $ show err
   Right x  -> g4_exp x

g4_exp :: [G4P.G4] -> TH.Q TH.Exp -- exp :: G4
g4_exp ast = do
  asts <- mapM lift ast
  [| toAllstarGrammar $(return $ ListE asts) |]

-- TODO Mutator and Predicated fn' cases
-- TODO Lex case
toAllstarGrammar :: [G4P.G4] -> (AG.NonTerminal -> AG.Grammar s)
toAllstarGrammar grammarMems =
  let
      toNonTerms :: [Either G4P.GTerm G4P.GNonTerm] -> [String]
      toNonTerms [] = []
      toNonTerms ((Left (G4P.GTerm s)) : rest ) = (s : toNonTerms rest)
      toNonTerms (_:rest) = toNonTerms rest
      toElems :: [Either G4P.GTerm G4P.GNonTerm] -> [AG.ProdElem]
      toElems ((Left  (G4P.GTerm    s)) : rest) = (AG.T  s : toElems rest)
      toElems ((Right (G4P.GNonTerm s)) : rest) = (AG.NT s : toElems rest)
      fn (G4P.Grammar name) (g,c) = (g,c)
      fn (G4P.Prod pName patterns) (g,c) =
        let fn' (G4P.PRHS alphas Nothing Nothing) (g',c') =
              (g' { AG.ns = (AG.ns g') `Set.union` (Set.singleton pName)
                  , AG.ts = (AG.ts g') `Set.union` (Set.fromList $ toNonTerms alphas)
                  , AG.ps = (pName, AG.Prod $ toElems alphas) : (AG.ps g')
                  }, c)
            fn' (G4P.PRHS alphas (Just pre) Nothing) (g',c') = (g',c')    -- TODO
            fn' (G4P.PRHS alphas Nothing (Just mut)) (g',c') = (g',c')    -- TODO
            fn' (G4P.PRHS alphas (Just pre) (Just mut)) (g',c') = (g',c') -- TODO
        in  foldr fn' (g,c) patterns
      fn (G4P.Lex annotation lName pattern) (g,c) = undefined             -- TODO
  in (\s0' ->
              let (g,_) = foldr fn (AG.G Set.empty Set.empty [] s0' Set.empty Set.empty, 0) grammarMems
              in  g
     )
