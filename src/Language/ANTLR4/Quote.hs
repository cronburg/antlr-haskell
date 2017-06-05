{-# LANGUAGE QuasiQuotes, TemplateHaskell, ScopedTypeVariables #-}
module Language.ANTLR4.Quote
( antlr4
) where
import Prelude hiding (exp, init)
import System.IO.Unsafe (unsafePerformIO)
import Data.Char (toLower, toUpper)

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (lift, Exp(..))
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Control.Monad (mapM)
import qualified Language.ANTLR4.Syntax as G4S
import qualified Language.ANTLR4.Parser as G4P
import Text.ANTLR.Allstar.Grammar

import Text.ANTLR.Set (Set(..))
import qualified Text.ANTLR.Set as Set

antlr4 :: QuasiQuoter
antlr4 =  QuasiQuoter
  (error "parse exp")
  (error "parse pattern")
  (error "parse type")
  aparse --(error "parse decl")

--   parser in quasiquotation monad
aparse :: String -> TH.Q [TH.Dec]
aparse input = do
 -- TODO: replace bad error showing with
 --       debugging information (filename, line #, column) in parser
 loc <- TH.location
 let fileName = TH.loc_filename loc
 let (line,column) = TH.loc_start loc

 case G4P.parseANTLR fileName line column input of
   Left err -> unsafePerformIO $ fail $ show err
   Right x  -> g4_decls x

g4_decls :: [G4S.G4] -> TH.Q [TH.Dec] -- exp :: G4
g4_decls ast = let
    
    terminals :: [String]
    terminals = concatMap getTs ast

    nonterms  :: [String]
    nonterms  = concatMap getNTs ast

    justTerms :: [G4S.ProdElem] -> [String]
    justTerms [] = []
    justTerms (G4S.GTerm s:as) = mkUpper s : justTerms as
    justTerms (_:as) = justTerms as

    justNonTerms :: [G4S.ProdElem] -> [String]
    justNonTerms [] = []
    justNonTerms (G4S.GNonTerm s:as) = mkUpper s : justNonTerms as
    justNonTerms (_:as) = justNonTerms as

    getTs :: G4S.G4 -> [String]
    getTs G4S.Prod{G4S.patterns = ps} = concatMap (justTerms . G4S.alphas) ps
    getTs _ = []

    getNTs :: G4S.G4 -> [String]
    getNTs G4S.Prod{G4S.patterns = ps} = concatMap (justNonTerms . G4S.alphas) ps
    getNTs _ = []

    grammarName :: [G4S.G4] -> String
    grammarName [] = error "Grammar missing a name"
    grammarName (G4S.Grammar{G4S.gName = gName}:_) = gName
    grammarName (_:xs) = grammarName xs

    ntDataName = grammarName ast ++ "NT"
    tDataName  = grammarName ast ++ "T"

    ntDataType = DataD [] (mkName ntDataName) [] Nothing (map (\s -> NormalC (mkName s) []) nonterms)  []
    tDataType  = DataD [] (mkName tDataName)  [] Nothing (map (\s -> NormalC (mkName s) []) terminals) []

    toElem :: G4S.ProdElem -> TH.ExpQ
    toElem (G4S.GTerm s)    = [| T  $(return $ ConE $ mkName $ mkUpper s) |]
    toElem (G4S.GNonTerm s) = [| NT $(return $ ConE $ mkName $ mkUpper s) |]

    mkProd :: String -> [TH.ExpQ] -> TH.ExpQ
    mkProd n es = [| Production $(return $ ConE $ mkName $ mkUpper n) $ Prod Pass $(listE es) |]

    getProds :: [G4S.G4] -> [TH.ExpQ]
    getProds [] = []
    getProds (G4S.Prod {G4S.pName = n, G4S.patterns = ps}:xs)
      = map (mkProd n . map toElem . G4S.alphas) ps ++ getProds xs
    getProds (_:xs) = getProds xs

    -- The first NonTerminal in the grammar (TODO: head of list)
    s0 :: TH.ExpQ
    s0 = return $ ConE $ mkName $ mkUpper $ head $ nonterms

    grammar = [| defaultGrammar $(s0)
      { ns = Set.fromList [minBound .. maxBound :: $(return (ConT $ mkName ntDataName))]
      , ts = Set.fromList [minBound .. maxBound :: $(return (ConT $ mkName tDataName))]
      , ps = Set.fromList $(listE $ getProds ast)
      } |]

    mkLower [] = []
    mkLower (a:as) = toLower a : as

    mkUpper [] = []
    mkUpper (a:as) = toUpper a : as

  in do g <- grammar
        return
          [ FunD (mkName $ mkLower $ grammarName ast) [Clause [] (NormalB g) []]
          ] 

{-
-- TODO Mutator and Predicated fn' cases
-- TODO Lex case
toAllstarGrammar ::
  forall s. [G4S.G4] -> (G4S.GNonTerm -> Grammar s G4S.GNonTerm G4S.GTerm)
toAllstarGrammar grammarMems s0' =
  let
      -- Grab only the terminals from the RHS of a production:
      getJustTerms :: [G4S.ProdElem] -> [G4S.GTerm]
      getJustTerms [] = []
      getJustTerms ((Left s) : rest ) = (s : getJustTerms rest)
      getJustTerms (_:rest) = getJustTerms rest
      
      toElems :: [G4S.ProdElem] -> [ProdElem G4S.GNonTerm G4S.GTerm]
      toElems ((Left   t) : rest) = (T   t : toElems rest)
      toElems ((Right nt) : rest) = (NT nt : toElems rest)

      fn (G4S.Grammar name) (g,c) = (g,c)
      fn (G4S.Prod pName patterns) (g,c) =
        let fn' (G4S.PRHS alphas Nothing Nothing) (g',c') =
              (g' { ns = (ns g') `Set.union` (Set.singleton $ G4S.GNonTerm pName)
                  , ts = (ts g') `Set.union` (Set.fromList $ getJustTerms alphas)
                  , ps = Production (G4S.GNonTerm pName) (Prod Pass $ toElems alphas) : (ps g')
                  }, c)
            fn' (G4S.PRHS alphas (Just pre) Nothing) (g',c') = (g',c')    -- TODO
            fn' (G4S.PRHS alphas Nothing (Just mut)) (g',c') = (g',c')    -- TODO
            fn' (G4S.PRHS alphas (Just pre) (Just mut)) (g',c') = (g',c') -- TODO
        in  foldr fn' (g,c) patterns
      fn (G4S.Lex annotation lName pattern) (g,c) = undefined             -- TODO
  in let (g,_) = foldr fn (G Set.empty Set.empty [] s0' Set.empty Set.empty, 0) grammarMems
     in  g
-}

