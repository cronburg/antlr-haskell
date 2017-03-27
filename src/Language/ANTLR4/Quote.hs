{-# LANGUAGE QuasiQuotes, TemplateHaskell, ScopedTypeVariables #-}
module Language.ANTLR4.Quote
( antlr4
) where
import Prelude hiding (exp, init)
import System.IO.Unsafe (unsafePerformIO)

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax (lift, Exp(..))
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Control.Monad (mapM)
import qualified Language.ANTLR4.Syntax as G4S
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

g4_exp :: [G4S.G4] -> TH.Q TH.Exp -- exp :: G4
g4_exp ast = do
  asts <- mapM lift ast
  [| toAllstarGrammar $(return $ ListE asts) |]

-- TODO Mutator and Predicated fn' cases
-- TODO Lex case
toAllstarGrammar ::
  forall s. [G4S.G4] -> (G4S.GNonTerm -> AG.Grammar s G4S.GNonTerm G4S.GTerm)
toAllstarGrammar grammarMems =
  let
      -- Grab only the terminals from the RHS of a production:
      getJustTerms :: [Either G4S.GTerm G4S.GNonTerm] -> [G4S.GTerm]
      getJustTerms [] = []
      getJustTerms ((Left s) : rest ) = (s : getJustTerms rest)
      getJustTerms (_:rest) = getJustTerms rest
      
      toElems :: [Either G4S.GTerm G4S.GNonTerm] -> [AG.ProdElem G4S.GNonTerm G4S.GTerm]
      toElems ((Left   t) : rest) = (AG.T   t : toElems rest)
      toElems ((Right nt) : rest) = (AG.NT nt : toElems rest)

      fn (G4S.Grammar name) (g,c) = (g,c)
      fn (G4S.Prod pName patterns) (g,c) =
        let fn' (G4S.PRHS alphas Nothing Nothing) (g',c') =
              (g' { AG.ns = (AG.ns g') `Set.union` (Set.singleton $ G4S.GNonTerm pName)
                  , AG.ts = (AG.ts g') `Set.union` (Set.fromList $ getJustTerms alphas)
                  , AG.ps = (G4S.GNonTerm pName, AG.Prod $ toElems alphas) : (AG.ps g')
                  }, c)
            fn' (G4S.PRHS alphas (Just pre) Nothing) (g',c') = (g',c')    -- TODO
            fn' (G4S.PRHS alphas Nothing (Just mut)) (g',c') = (g',c')    -- TODO
            fn' (G4S.PRHS alphas (Just pre) (Just mut)) (g',c') = (g',c') -- TODO
        in  foldr fn' (g,c) patterns
      fn (G4S.Lex annotation lName pattern) (g,c) = undefined             -- TODO
  in (\s0' ->
              let (g,_) = foldr fn (AG.G Set.empty Set.empty [] s0' Set.empty Set.empty, 0) grammarMems
              in  g
     )
