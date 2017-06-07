{-# LANGUAGE QuasiQuotes, TemplateHaskell, ScopedTypeVariables, DataKinds #-}
module Language.ANTLR4.Quote
( antlr4, ProdElem(..)
) where
import Prelude hiding (exp, init)
import System.IO.Unsafe (unsafePerformIO)
import Data.List (nub, elemIndex)
import Data.Char (toLower, toUpper, isLower, isUpper)
import Data.Maybe (fromJust)

import qualified Debug.Trace as D

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

    -- Ordered (arbitrary) list of the terminal literals found in production
    -- rules of the grammar:
    --terminalLiterals :: [String]
    --terminalLiterals = nub $ concatMap getTLs ast

    -- Get Terminal Literals
    --getTLs :: G4S.G4 -> [String]
    --getTLs G4S.Prod{G4S.patterns = ps} = concatMap (justLiterals . G4S.alphas) ps
    --getTLs _ = []

    --justLiterals :: [G4S.ProdElem] -> [String]
    --justLiterals [] = []
    --justLiterals (

    terminals :: [String]
    terminals = nub $ concatMap getTs ast

    nonterms  :: [String]
    nonterms  = nub $ concatMap getNTs ast

    justTerms :: [G4S.ProdElem] -> [String]
    justTerms [] = []
    justTerms (G4S.GTerm s:as) = mkUpper s : justTerms as
    justTerms (_:as) = justTerms as

    justNonTerms :: [G4S.ProdElem] -> [String]
    justNonTerms [] = []
    justNonTerms (G4S.GNonTerm s:as)
      | (not . null) s && isLower (head s) = mkUpper s : justNonTerms as
      | otherwise = justNonTerms as
    justNonTerms (_:as) = justNonTerms as

    getTs :: G4S.G4 -> [String]
    getTs G4S.Prod{G4S.patterns = ps} = concatMap (justTerms . G4S.alphas) ps
    getTs _ = []

    getNTs :: G4S.G4 -> [String]
    getNTs G4S.Prod{G4S.pName = pName, G4S.patterns = ps} = mkUpper pName : concatMap (justNonTerms . G4S.alphas) ps
    getNTs _ = []

    grammarName :: [G4S.G4] -> String
    grammarName [] = error "Grammar missing a name"
    grammarName (G4S.Grammar{G4S.gName = gName}:_) = gName
    grammarName (_:xs) = grammarName xs

    ntDataName = grammarName ast ++ "NTSymbol"
    tDataName  = grammarName ast ++ "TSymbol"

    -- Things Symbols must derive:
    symbolDerives = map (ConT . mkName)
      [ "Eq", "Ord", "Show", "Hashable", "Generic", "Bounded", "Enum"]

    ntDataType = DataD [] (mkName ntDataName) [] Nothing (map (\s -> NormalC (mkName s) []) nonterms)  symbolDerives
    tDataType  =
      DataD []
        (mkName tDataName)  
        []
        Nothing 
        ((map (\s -> NormalC (mkName $ lookupTName s) []) terminals) ++ lexemeNames)
        symbolDerives

    lexemeNames :: [Con]
    lexemeNames = let
        lN :: G4S.G4 -> [String]
        lN G4S.Lex{G4S.lName = lName} = ["T_" ++ lName]
        lN _ = []
      
        lN' = concatMap lN ast

      in map (\s -> NormalC (mkName s) []) lN'

    lookupTName :: String -> String
    lookupTName s = "T_" ++
      (case s `elemIndex` terminals of
        Nothing -> s
        Just i  -> show i)

    -- TODO: how can I just quasiquote this?
    strBangType = (Bang NoSourceUnpackedness NoSourceStrictness, ConT $ mkName "String")

    mkCon = return . ConE . mkName . mkUpper

    toElem :: G4S.ProdElem -> TH.ExpQ
    toElem (G4S.GTerm s)    = [| $(mkCon "T")  $(mkCon $ lookupTName s) |] -- $(return $ LitE $ StringL s)) |]
    toElem (G4S.GNonTerm s)
      | (not . null) s && isLower (head s) = [| $(mkCon "NT") $(mkCon s) |]
      | otherwise = toElem (G4S.GTerm s)

    mkProd :: String -> [TH.ExpQ] -> TH.ExpQ
    mkProd n es = [| $(mkCon "Production") $(return $ ConE $ mkName $ mkUpper n) ($(mkCon "Prod") $(mkCon "Pass") $(listE es)) |]

    getProds :: [G4S.G4] -> [TH.ExpQ]
    getProds [] = []
    getProds (G4S.Prod {G4S.pName = n, G4S.patterns = ps}:xs)
      = map (mkProd n . map toElem . G4S.alphas) ps ++ getProds xs
    getProds (_:xs) = getProds xs

    -- The first NonTerminal in the grammar (TODO: head of list)
    s0 :: TH.ExpQ
    s0 = return $ ConE $ mkName $ mkUpper $ head nonterms

    grammar gTy = [| (defaultGrammar $(s0) :: $(return gTy))
      { ns = Set.fromList [minBound .. maxBound :: $(return (ConT $ mkName ntDataName))]
      , ts = Set.fromList [minBound .. maxBound :: $(return (ConT $ mkName tDataName))]
      , ps = $(listE $ getProds ast)
      } |]

    grammarTy = [t| forall s. Grammar s $(return $ ConT $ mkName ntDataName) $(return $ ConT $ mkName tDataName) |]

    mkLower [] = []
    mkLower (a:as) = toLower a : as

    mkUpper [] = []
    mkUpper (a:as) = toUpper a : as

    {----------------------- Tokenizer -----------------------}

    tokenNameType = TySynD (mkName "TokenName") [] (ConT $ mkName tDataName)
    
    tokenValueType = DataD [] (mkName "TokenValue") [] Nothing [] []

  -- IMPORTANT: Creating type variables in two different haskell type
  -- quasiquoters with the same variable name produces two (uniquely) named type
  -- variables. In order to achieve the same type variable you need to run one
  -- in the Q monad first then pass the resulting type to other parts of the
  -- code that need it (thus capturing the type variable).
  in do gTy <- grammarTy
        g   <- grammar gTy
        return
          [ ntDataType, tDataType
          , SigD (mkName $ mkLower $ grammarName ast) gTy
          , FunD (mkName $ mkLower $ grammarName ast) [Clause [] (NormalB g) []]
          , tokenNameType, tokenValueType
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

