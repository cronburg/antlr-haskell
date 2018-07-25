{-# LANGUAGE  QuasiQuotes, TemplateHaskell, ScopedTypeVariables, DataKinds,
              LambdaCase, FlexibleContexts #-}
module Language.ANTLR4.Boot.Quote
( antlr4, ProdElem(..), g4_decls, mkLRParser
) where
import Prelude hiding (exp, init)
import System.IO.Unsafe (unsafePerformIO)
import Data.List (nub, elemIndex, groupBy, sortBy, sort)
import Data.Ord (comparing)
import Data.Char (toLower, toUpper, isLower, isUpper)
import Data.Maybe (fromJust, catMaybes)

import qualified Debug.Trace as D

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (lift, Exp(..))
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import qualified Language.Haskell.Meta as LHM

import Control.Monad (mapM)
import qualified Language.ANTLR4.Boot.Syntax as G4S

--import qualified Language.ANTLR4.Boot.Parser as G4P
import qualified Language.ANTLR4.Boot.SplicedParser as G4P

import qualified Language.ANTLR4.Regex  as G4R
import Text.ANTLR.Grammar
import Text.ANTLR.Parser (AST(..), StripEOF(..))
import Text.ANTLR.Pretty
import Text.ANTLR.Lex.Tokenizer as T
import Text.ANTLR.LR as LR
import qualified Text.ANTLR.Allstar as ALL
import qualified Text.ANTLR.LL1 as LL
import qualified Text.ANTLR.Set as S

import Text.ANTLR.Set (Set(..))
import qualified Text.ANTLR.Set as Set
import qualified Text.ANTLR.Lex.Regex as R

--trace s = D.trace   ("[Language.ANTLR4.Boot.Quote] " ++ s)
--traceM s = D.traceM ("[Language.ANTLR4.Boot.Quote] " ++ s)

trace s x = x
traceM s x = x

haskellParseExp :: (Monad m) => String -> m TH.Exp
haskellParseExp s = case LHM.parseExp s of
  Left err    -> error err
  Right expTH -> return expTH

haskellParseType :: (Monad m) => String -> m TH.Type
haskellParseType s = case LHM.parseType s of
  Left err   -> trace s (error err)
  Right tyTH -> return tyTH

type2returnType :: TH.Type -> TH.Type
type2returnType = let
    
    t2rT :: TH.Type -> TH.Type
    t2rT (ForallT xs ys t) = t2rT t
    t2rT ((AppT (AppT ArrowT from) to)) = t2rT to
    t2rT t@(VarT _)        = t
    t2rT t@(AppT ListT as) = t
    t2rT t@(ConT _)        = t
    t2rT t@(AppT (ConT _) _) = t
    t2rT x = error (show x)

  in t2rT

info2returnType :: Info -> TH.Type
info2returnType i = let
  
  in case i of
      (VarI _ t _) -> type2returnType t
      _ -> error (show i)

--trace s = id
--traceM = return

antlr4 :: QuasiQuoter
antlr4 =  QuasiQuoter
  (error "parse exp")
  (error "parse pattern")
  (error "parse type")
  aparse --(error "parse decl")

-- e.g. Named ("Num", "Int") where 'Num' was a G4 lexeme and 'Int' was given
-- as a directive specifying the desired type to read (must instance Read).
data LexemeType =
    Literal Int           -- A literal lexeme somewhere in the grammar, e.g. ';'
  | AString               -- Type was unspecified in the G4 lexeme or specified as a String
  | Named String TH.TypeQ -- Type was specified as a directive in the G4 lexeme

aparse :: String -> TH.Q [TH.Dec]
aparse input = do
  loc <- TH.location
  let fileName = TH.loc_filename loc
  let (line,column) = TH.loc_start loc

  case G4P.parseANTLR input of
    r@(LR.ResultAccept ast) -> codeGen r
    LR.ResultSet    s   ->
      if S.size s == 1
        then codeGen (S.findMin s)
        else error $ pshow' s
    err                 -> error $ pshow' err

codeGen (LR.ResultAccept ast) = g4_decls $ G4P.ast2decls ast

{-
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
-}

data BaseType = List | Mybe
  deriving (Eq, Ord, Show)

baseType (G4S.Regular '?') = Mybe
baseType (G4S.Regular '*') = List
    
-- Find the (first) name of the grammar
grammarName :: [G4S.G4] -> String
grammarName [] = error "Grammar missing a name"
grammarName (G4S.Grammar{G4S.gName = gName}:_) = gName
grammarName (_:xs) = grammarName xs
    
mkLower [] = []
mkLower (a:as) = toLower a : as

mkUpper [] = []
mkUpper (a:as) = toUpper a : as

justGrammarTy ast s = [t| Grammar $(s) $(ntConT ast) $(tConT ast) |]
justGrammarTy' ast s = [t| Grammar $(s) $(ntConT ast) (StripEOF (Sym $(tConT ast))) |]

ntConT ast = conT $ mkName $ ntDataName ast
tConT  ast = conT $ mkName $ tDataName ast

ntDataName ast = gName ast ++ "NTSymbol"
tDataName  ast = gName ast ++ "TSymbol"

gName ast = grammarName ast


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

    -- A list of all the G4 literal terminals scattered across production rules
    terminalLiterals :: [String]
    terminalLiterals = (nub $ concatMap getTerminals ast)

    -- A list of all the terminals in the grammar (both literal G4 terminals and
    -- G4 lexical terminals)
    terminals :: [String]
    terminals = terminalLiterals ++ lexemeNames

    -- A list of all the G4 lexeme names specified in the grammar
    lexemeNames :: [String]
    lexemeNames = map fst lexemeTypes

    nonterms  :: [String]
    nonterms  = nub $ concatMap getNTs ast

    -- Find all terminals *literals* in a production like '(' and ')' and ';'
    justTerms :: [G4S.ProdElem] -> [String]
    justTerms [] = []
    justTerms ((G4S.GTerm _ s) : as) = s : justTerms as
    justTerms (_:as) = justTerms as

    -- Find all nonterminals in a production like 'exp' and 'decl'
    justNonTerms :: [G4S.ProdElem] -> [String]
    justNonTerms [] = []
    justNonTerms (G4S.GNonTerm _ s:as)
      | (not . null) s && isLower (head s) = s : justNonTerms as
      | otherwise = justNonTerms as
    justNonTerms (_:as) = justNonTerms as

    -- Find all terminal literals in a G4 grammar rule like '(' and ')' and ';'
    getTerminals :: G4S.G4 -> [String]
    getTerminals G4S.Prod{G4S.patterns = ps} = concatMap (justTerms . G4S.alphas) ps
    getTerminals _ = []

    -- Find all the nonterminals referenced in the production(s) of the given grammar rule
    getNTs :: G4S.G4 -> [String]
    getNTs G4S.Prod{G4S.pName = pName, G4S.patterns = ps} = pName : concatMap (justNonTerms . G4S.alphas) ps
    getNTs _ = []

    -- Things Symbols must derive:
    symbolDerives = derivClause Nothing $ map (conT . mkName)
      [ "Eq", "Ord", "Show", "Hashable", "Generic", "Bounded", "Enum", "Data", "Lift"]

    -- Nonterminal symbol data type (enum) for this grammar:
    ntDataDeclQ :: DecQ
    ntDataDeclQ =
      dataD (cxt [])
      (mkName $ ntDataName ast)
      []
      Nothing
      (map (\s -> normalC (mkName $ "NT_" ++ s) []) $ nonterms ++ regexNonTermSymbols)
      [symbolDerives]
    
    -- E.g. ['(', ')', ';', 'exp', 'decl']
    allLexicalSymbols :: [String]
    allLexicalSymbols = map (lookupTName "") terminalLiterals ++ lexemeNames

    -- E.g. [('(', Literal 0), (')', Literal 1), (';', Literal 2), ('exp',
    -- AString), ('decl', AString')]
    allLexicalTypes :: [(String, LexemeType)]
    allLexicalTypes = (map lookupLiteralType terminalLiterals) ++ lexemeTypes

    -- E.g. [('(', Literal 0), ...]
    lookupLiteralType :: String -> (String, LexemeType)
    lookupLiteralType s =
      case s `elemIndex` terminalLiterals of
        Nothing -> undefined
        Just i  -> (s, Literal i)

    -- Terminal symbol data type (enum) for this grammar:
    tDataDeclQ :: DecQ
    tDataDeclQ =
      dataD (cxt [])
        (mkName $ tDataName ast)
        []
        Nothing 
        (map (\s -> normalC (mkName s) []) (map ("T_" ++) allLexicalSymbols))
        --(\s -> normalC (mkName $ lookupTName "T_" s) []) lexemes) ++ (lexemeNames "T_"))
        [symbolDerives]

    -- THIS EXCLUDES LEXEME FRAGMENTS:
    -- e.g. [('UpperID', AString), ('SetChar', Named String)]
    lexemeTypes :: [(String, LexemeType)]
    lexemeTypes = let

        nullID (G4S.UpperD xs)  = null xs
        nullID (G4S.LowerD xs)  = null xs
        nullID (G4S.HaskellD _) = False

        lN :: G4S.G4 -> [(String, LexemeType)]
        lN (G4S.Lex{G4S.annotation = Nothing, G4S.lName = lName, G4S.pattern = G4S.LRHS{G4S.directive = Nothing}}) = [(lName, AString)]
        lN (G4S.Lex{G4S.annotation = Nothing, G4S.lName = lName, G4S.pattern = G4S.LRHS{G4S.directive = Just s}})
          | s == (G4S.UpperD "String") = [(lName, AString)]
          | nullID s          = [(lName, AString)] -- quirky G4 parser
          | otherwise = case s of
              (G4S.UpperD s) -> [(lName, Named s (conT $ mkName s))]
              (G4S.LowerD s)     -> [(lName, Named s (info2returnType <$> reify (mkName s)))]
              (G4S.HaskellD s)   -> [] -- TODO?
        lN _ = []
      in concatMap lN ast
      --map (\s -> normalC (mkName s) []) lN'

    -- Map from a terminal's syntax to the name of the data type instance from
    -- tDataDeclQ:
    lookupTName :: String -> String -> String
    lookupTName pfx s = pfx ++
      (case s `elemIndex` terminalLiterals of
        Nothing -> s
        Just i  -> show i)

    strBangType = (defBang, conT $ mkName "String")

    mkCon   = conE . mkName . mkUpper
    mkConNT = conE . mkName . ("NT_" ++)

    -- 
    genTermAnnotProds :: [G4S.G4] -> [G4S.G4]
    genTermAnnotProds [] = []
    genTermAnnotProds (G4S.Prod {G4S.pName = n, G4S.patterns = ps}:xs) = let
        
        withAlphas newName d a = G4S.Prod {G4S.pName = newName, G4S.patterns =
          [ G4S.PRHS
              { G4S.pred        = Nothing
              , G4S.alphas      = a
              , G4S.mutator     = Nothing
              , G4S.pDirective  = Just d
              }
          ]}
        
        gTAP :: G4S.ProdElem -> [G4S.G4]
        gTAP (G4S.GNonTerm (G4S.Regular '?') nt) = trace (show nt)
          [ withAlphas (nt ++ "_quest") (G4S.UpperD "Maybe") [G4S.GNonTerm G4S.NoAnnot nt]
          , withAlphas (nt ++ "_quest") (G4S.UpperD "Maybe") [] -- epsilon
          ]
        gTAP (G4S.GNonTerm (G4S.Regular '*') nt) =
          [ withAlphas (nt ++ "_star")  (G4S.LowerD "cons")  [G4S.GNonTerm G4S.NoAnnot nt, G4S.GNonTerm G4S.NoAnnot (nt ++ "_star")]
          , withAlphas (nt ++ "_star")  (G4S.LowerD "list")  [G4S.GNonTerm G4S.NoAnnot nt]
          , withAlphas (nt ++ "_star")  (G4S.LowerD "list")  []
          ]
        gTAP (G4S.GNonTerm (G4S.Regular '+') nt) =
          [ withAlphas (nt ++ "_plus")  (G4S.LowerD "cons")  [G4S.GNonTerm G4S.NoAnnot nt, G4S.GNonTerm G4S.NoAnnot (nt ++ "_plus")]
          , withAlphas (nt ++ "_plus")  (G4S.LowerD "list")  [G4S.GNonTerm G4S.NoAnnot nt]
          ]
        gTAP (G4S.GNonTerm G4S.NoAnnot nt) = []
        gTAP (G4S.GTerm _ t) = []
        gTAP term = error $  show term
      in concat (concatMap (map gTAP) (map G4S.alphas ps)) ++ genTermAnnotProds xs
    genTermAnnotProds (_:xs) = genTermAnnotProds xs

    annotName G4S.NoAnnot s = s
    annotName (G4S.Regular '?') s = s ++ "_quest"
    annotName (G4S.Regular '*') s = s ++ "_star"
    annotName (G4S.Regular '+') s = s ++ "_plus"
    annotName (G4S.Regular c)   s = s ++ [c] -- TODO: warning on unknown character annotation

    annotName' (G4S.GTerm annot s) = annotName annot s
    annotName' (G4S.GNonTerm annot s) = annotName annot s

    regexNonTermSymbols = let
        
        rNTS (G4S.Prod {G4S.patterns = ps}) = Just $ map G4S.alphas ps
        rNTS _ = Nothing
    
      in nub $ map annotName' $ filter (not . G4S.isNoAnnot . G4S.annot) (concat $ concat $ catMaybes $ map rNTS ast)

    toElem :: G4S.ProdElem -> TH.ExpQ
    toElem (G4S.GTerm annot s)    = [| $(mkCon "T")  $(mkCon $ lookupTName "T_" (annotName annot s)) |] -- $(return $ LitE $ StringL s)) |]
    toElem (G4S.GNonTerm annot s)
      | (not . null) s && isLower (head s) = [| $(mkCon "NT") $(mkConNT (annotName annot s)) |]
      | otherwise = toElem (G4S.GTerm G4S.NoAnnot s)

    mkProd :: String -> [TH.ExpQ] -> TH.ExpQ
    mkProd n [] = [| $(mkCon "Production") $(conE $ mkName $ "NT_" ++ n) ($(mkCon "Prod") $(mkCon "Pass") [Eps]) |]
    mkProd n es = [| $(mkCon "Production") $(conE $ mkName $ "NT_" ++ n) ($(mkCon "Prod") $(mkCon "Pass") $(listE es)) |]

    getProds :: [G4S.G4] -> [TH.ExpQ]
    getProds [] = []
    getProds (G4S.Prod {G4S.pName = n, G4S.patterns = ps}:xs)
      = map (mkProd n . map toElem . G4S.alphas) ps ++ getProds xs
    getProds (_:xs) = getProds xs

    -- The first NonTerminal in the grammar (TODO: head of list)
    s0 :: TH.ExpQ
    s0 = conE $ mkName $ "NT_" ++ head nonterms

    grammar gTy = [| (defaultGrammar $(s0) :: $(return gTy))
      { ns = Set.fromList [minBound .. maxBound :: $(ntConT ast)]
      , ts = Set.fromList [minBound .. maxBound :: $(tConT ast)]
      , ps = $(listE $ getProds $ ast ++ genTermAnnotProds ast)
      } |]

    --grammarTy s = [t| forall $(s). (Prettify $(s)) => $(justGrammarTy s) |]
    grammarTy s = [t| (Prettify $(s)) => $(justGrammarTy ast s) |]

    {----------------------- Tokenizer -----------------------}

    tokenNameTypeQ = tySynD (mkName "TokenName") [] (conT $ mkName $ tDataName ast)
    
    defBang = bang noSourceUnpackedness noSourceStrictness

    lexemeValueDerives = derivClause Nothing $ map (conT . mkName)
      ["Show", "Ord", "Eq", "Generic", "Hashable", "Data"]

    -- 
    lexemeTypeConstructors = let
        nullD (G4S.UpperD s) = null s
        nullD (G4S.LowerD s) = null s
        nullD (G4S.HaskellD s) = null s

        lTC (i, lex@(G4S.Lex{G4S.annotation = Nothing, G4S.lName = lName, G4S.pattern = G4S.LRHS{G4S.directive = Just d}}))
          | null lName       = error $ "null lexeme name: " ++ show lex
          | nullD d          = Just $ normalC (mkName $ "V_" ++ lName) [bangType defBang (conT $ mkName "String")]
          | otherwise = case d of
              (G4S.UpperD d) -> Just $ normalC (mkName $ "V_" ++ lName) [bangType defBang (conT $ mkName d)]
              (G4S.LowerD d) -> Just $ do
                  info <- reify $ mkName d
                  normalC (mkName $ "V_" ++ lName) [bangType defBang (return $ info2returnType info)]
                --Just $ [|| $$(haskellParseExp d) ||] --error $ "unimplemented use of function in G4 directive: " ++ show d
              (G4S.HaskellD s) -> Nothing -- TODO?
        lTC _ = Nothing
      in   ((catMaybes $ map lTC (zip [0 .. length ast - 1] ast))
        ++ (map (\s -> normalC (mkName $ lookupTName "V_" s) []) terminalLiterals))

    tokenValueTypeQ =
      dataD (cxt []) (mkName "TokenValue") [] Nothing
      lexemeTypeConstructors
      [lexemeValueDerives]

    mkTyVar s f = return $ f $ mkName s

    lookupTokenFncnDecl = let
        lTFD t = clause [litP $ stringL t]
                  (normalB $ [| Token   $(conE $ mkName   $ lookupTName "T_" t)
                                        $(conE $ mkName   $ lookupTName "V_" t)
                                        $(litE $ integerL $ fromIntegral $ length t) |])
                  []
      in funD (mkName "lookupToken")
        (  map lTFD terminalLiterals
        ++ [clause [varP $ mkName "s"]
            (normalB $ [| error ("Error: '" ++ s ++ "' is not a token") |])
            []]
        )

    -- Construct the function that takes in a lexeme (string) and the token name
    -- (T_*) and constructs a token value type instance using 'read' where
    -- appropriate based on the directives given in the grammar.
    lexeme2ValueQ lName = let
        
        l2VQ (_, Literal i) =
          clause [varP lName, conP (mkName $ "T_" ++ show i) []]
          (normalB [| $(conE $ mkName $ "V_" ++ show i) |]) []
        l2VQ (s, AString)   =
          clause [varP lName, conP (mkName $ "T_" ++ s) []]
          (normalB [| $(conE $ mkName $ "V_" ++ s) $(varE lName) |]) []
        l2VQ (s, Named n t)
          | isLower (head n) =
              clause [varP lName, conP (mkName $ "T_" ++ s) []]
              (normalB [| $(conE $ mkName $ "V_" ++ s) (trace $(varE lName) ($(varE $ mkName n) $(varE lName) :: $t)) |]) []
          | otherwise =
              clause [varP lName, conP (mkName $ "T_" ++ s) []]
              (normalB [| $(conE $ mkName $ "V_" ++ s) (trace $(varE lName) (read $(varE lName) :: $t)) |]) []
              
              --info <- reify $ mkName d
              --normalC (mkName $ "V_" ++ lName) [bangType defBang (return $ info2returnType info)]
      
      in funD (mkName "lexeme2value") (map l2VQ allLexicalTypes)

    -- Convert a G4 regex into the backend regex type (for constructing token
    -- recognizers as DFAs):
    convertRegex :: (Show c) => (String -> G4R.Regex c) -> G4R.Regex c -> R.Regex c
    convertRegex getNamedR = let
        cR G4R.Epsilon       = R.Epsilon
        cR (G4R.Literal [])  = R.Epsilon
        cR (G4R.Literal [c]) = R.Symbol c
        cR (G4R.Literal cs)  = R.Literal cs
        cR (G4R.Union rs)    = R.MultiUnion $ map cR rs
        cR (G4R.Concat rs)   = R.Concat $ map cR rs
        cR (G4R.Kleene r)    = R.Kleene $ cR r
        cR (G4R.PosClos r)   = R.PosClos $ cR r
        cR (G4R.Question r)  = R.Question $ cR r
        cR (G4R.CharSet cs)  = R.Class cs
        cR (G4R.Negation (G4R.CharSet cs)) = R.NotClass cs
        cR (G4R.Negation (G4R.Literal s)) = R.NotClass s
        cR r@(G4R.Negation _) = error $ "unimplemented: " ++ show r
        cR (G4R.Named n)    = convertRegex getNamedR $ getNamedR n
      in cR

    getNamedRegex :: String -> G4R.Regex Char
    getNamedRegex n = let
        -- Only the lexeme (fragments) with the given name:
        gNR (G4S.Lex{G4S.annotation = Just G4S.Fragment, G4S.lName = lName}) = lName == n
        gNR _ = False
      in case filter gNR ast of
            [] -> error $ "No fragment named '" ++ n ++ "'"
            [(G4S.Lex{G4S.pattern = G4S.LRHS{G4S.regex = r}})] -> r
            xs -> error $ "Too many fragments named '" ++ n ++ "', i.e.: " ++ show xs

    -- Make the list of tuples containing regexes, one for each terminal.
    mkRegexesQ = let
        mkLitR :: String -> ExpQ
        mkLitR s = [| ($( conE $ mkName $ lookupTName "T_" s)
                        , $(lift $ convertRegex getNamedRegex $ G4R.Literal s)) |]

        mkLexR :: G4S.G4 -> Maybe ExpQ
        mkLexR (G4S.Lex{G4S.annotation = Nothing, G4S.lName = lName, G4S.pattern = G4S.LRHS{G4S.regex = r}}) = Just
          [| ($(conE $ mkName $ lookupTName "T_" lName), $(lift $ convertRegex getNamedRegex r)) |]
        mkLexR _ = Nothing
      in valD (varP $ mkName $ mkLower $ gName ast ++ "Regexes")
          (normalB $ listE (map mkLitR terminalLiterals ++ (catMaybes $ map mkLexR ast)))
          []

    prettyTFncnQ fncnName = let
        pTFLit lexeme =
          clause [conP (mkName $ lookupTName "T_" lexeme) []]
          (normalB [| pStr $(litE $ stringL $ "'" ++ lexeme ++ "'") |])
          []

        pTFName lexeme = 
          clause [conP (mkName $ lookupTName "T_" lexeme) []]
          (normalB [| pStr $(litE $ stringL $ lexeme) |])
          []
      in funD fncnName (map pTFLit terminalLiterals ++ map pTFName lexemeNames)

    prettyVFncnQ fncnName = let
        pVFLit lexeme =
          clause [conP (mkName $ lookupTName "V_" lexeme) []]
          (normalB [| pStr $(litE $ stringL $ "'" ++ lexeme ++ "'") |])
          []

        pVFName lexeme = 
          clause [conP (mkName $ lookupTName "V_" lexeme) [varP (mkName "v")]]
          (normalB [| pChr '\'' >> prettify v >> pChr '\'' |])
          []
      in funD fncnName (map pVFLit terminalLiterals ++ map pVFName lexemeNames)
    
    -- Pattern matches on an AST to produce a Maybe DataType
    ast2DTFncnsQ nameAST = let
        
        astFncnName s = mkName $ "ast2" ++ s
        
        a2d G4S.Lex{G4S.annotation = Nothing, G4S.lName  = _A, G4S.pattern = G4S.LRHS{G4S.directive = dir}}
          = Just [(mkName $ "ast2" ++ _A
                   ,[ clause  [ conP (mkName "Leaf")
                                [ conP (mkName $ "Token")
                                  [ wildP
                                  , conP (mkName $ lookupTName "V_" _A)
                                    [ varP $ mkName "t"]
                                  , wildP]]]
                              (normalB (varE $ mkName "t"))
                              []
                    ]
                  )]
        {-
        a2d G4S.Lex{G4S.lName  = _A, G4S.pattern = G4S.LRHS{G4S.directive = Just s}}
          | s == "String" = Just [funD (mkName $ "ast2" ++ _A) [ clause [] (normalB (varE $ mkName "id")) [] ]]
          | null s        = Just [funD (mkName $ "ast2" ++ _A) [ clause [] (normalB (varE $ mkName "id")) [] ]]
          | otherwise     = Just [funD (mkName $ "ast2" ++ _A) [ clause [] (normalB (varE $ mkName s)) [] ]]
        -}
        a2d G4S.Prod{G4S.pName = _A, G4S.patterns = ps} = let

          mkConP (G4S.GNonTerm annot nt)
            -- Some nonterminals are really terminal tokens (regular expressions):
            | isUpper (head nt)     = conP (mkName "T")  [conP (mkName $ lookupTName "T_" $ annotName annot nt) []]
            | otherwise             = conP (mkName "NT") [conP (mkName $ "NT_" ++ annotName annot nt) []]
          mkConP (G4S.GTerm annot t)   = conP (mkName "T")  [conP (mkName $ lookupTName "T_" $ annotName annot t) []]

          justStr (G4S.GNonTerm annot s) = annotName annot s
          justStr (G4S.GTerm    _     s) = s

          vars as = catMaybes
                    [ if G4S.isGNonTerm a
                        then Just (a, mkName $ "v" ++ show i ++ "_" ++ justStr a, varE $ mkName $ "ast2" ++ justStr a)
                        else Nothing
                    | (i, a) <- zip [0 .. length as] as
                    ]

          astListPattern as = listP $
                [ if G4S.isGNonTerm a
                    then varP  $ mkName $ "v" ++ show i ++ "_" ++ justStr a
                    else wildP
                | (i, a) <- zip [0 .. length as] as
                ]

          astAppRec b (alpha, varName, recName) = case G4S.annot alpha of
              G4S.NoAnnot       -> appE b (appE recName $ varE varName)
              (G4S.Regular '?') -> appE b (appE recName $ varE varName)
              -- TODO: Below two cases:
              (G4S.Regular '*') -> appE b (appE recName $ varE varName)
              (G4S.Regular '+') -> appE b (appE recName $ varE varName)
              otherwise         -> error $ show alpha

          clauses = [ clause  [ [p| AST $(conP (mkName $ "NT_" ++ _A) [])
                                     $(listP $ map mkConP as)
                                     $(astListPattern as)
                                |]
                                     -- $(listP $ map (varP . fst) $ vars as)
                              ]
                        (case (dir, vars as) of
                          (Just (G4S.UpperD d), vs) -> normalB $ foldl astAppRec (conE $ mkName d) vs
                          (Just (G4S.LowerD d), vs) -> normalB $ foldl astAppRec (varE $ mkName d) vs
                          (Just (G4S.HaskellD d), vs) -> normalB $ foldl astAppRec (haskellParseExp d) vs
                          (Nothing, [])   -> normalB $ tupE []
                          (Nothing, [(a,v0,rec)]) -> normalB $ appE rec (varE v0)
                          (Nothing, vs)           -> normalB $ tupE $ map (\(a,vN,rN) -> appE rN $ varE vN) vs
                        ) []
                    | G4S.PRHS{G4S.alphas = as, G4S.pDirective = dir} <- ps
                    ]
          
          retType = let
            rT G4S.PRHS{G4S.alphas = as, G4S.pDirective = dir}
              = case (dir, vars as) of
                  (Just (G4S.UpperD d), vs) ->
                      (do  i <- reify $ mkName d
                           (case i of
                                    DataConI _ t n -> return $ type2returnType t
                                    VarI n t _     -> return t
                                    TyConI (DataD _ n _ _ _ _) -> conT n
                                    other          -> error $ show other))
                  (Just (G4S.LowerD d), vs) -> info2returnType <$> reify (mkName d)
                  (Just (G4S.HaskellD d), vs) -> error "unimplemented" -- TODO if we ever add back the fncnSig below
                  (Nothing, [])         -> tupleT 0
                  (Nothing, [(a,v0,rec)]) -> tupleT 0
                  (Nothing, vs)         -> tupleT $ length vs
            in rT (head ps)

          fncnSig
            = do rT <- retType
                 (case rT of
                    ForallT vs c t  -> forallT vs (cxt []) [t| $(conT nameAST) -> $(return t) |]
                    t               -> forallT [] (cxt []) [t| $(conT nameAST) -> $(return t) |])

          in Just $ [ --sigD fncnName fncnSig
                      (astFncnName _A, clauses)
                    ]
        a2d _ = Nothing

        -- ast2* functions necessary to support '?', '+', and '*' in G4 syntax.
        -- This assumes productions look like how LL.removeEpsilons generates
        -- them
        --regex_a2d :: G4S.G4 -> [DecQ]
        regex_a2d :: G4S.G4 -> [(Name, [ClauseQ])]
        regex_a2d G4S.Prod{G4S.pName = _A, G4S.patterns = ps} = let
          
            clauses = [ clause [ [p| ast2 |] ] (normalB [| error (show ast2) |]) [] ]
      

            eachAlpha (G4S.GNonTerm (G4S.Regular '?') s) = let -- "_quest"
                ntName = "NT_" ++ s
              in
              [( astFncnName $ s ++ "_quest",
                [ -- First, the "zero or more" base case (returns a singleton list):
                  do  let n      = mkName ntName
                          nQuest = mkName $ ntName ++ "_quest"
                          base   = varE $ astFncnName s
                      param <- newName "param"
                      clause [ [p| AST $(conP nQuest []) [NT $(conP n [])] [$(varP param)] |] ]
                        (normalB [| Just ($(base) $(varE param)) |])
                        []
                , do  param <- newName "param"
                      clause [ [p| $(varP param) |] ]
                        (normalB [| error $ $(litE $ stringL ntName) ++ ": " ++ show $(varE param) |])
                        []
                ]
              )]
              {-
              [( astFncnName $ s ++ "_quest",
                [ do  param <- newName "param"
                      let base = varE $ astFncnName s
                      clause [ [p| $(varP param) |] ] (normalB [| Just $ $(base) ($(varE param) :: $(conT nameAST))|]) []
                ])]
              -}
            eachAlpha (G4S.GNonTerm (G4S.Regular '*') s) = let -- "_star"
                ntName = "NT_" ++ s

              in
              [( astFncnName $ s ++ "_star",
                [ -- First, the "zero or more" base case (returns a singleton list):
                  do  let n     = mkName ntName
                          nStar = mkName $ ntName ++ "_star"
                          base  = varE $ astFncnName s
                      param <- newName "param"
                      clause [ [p| AST $(conP nStar []) [NT $(conP n [])] [$(varP param)] |] ]
                        (normalB [| [$(base) $(varE param)] |])
                        []
                  -- Second, the "zero or more" recursive case (cons the current
                  -- thing onto a recursive call)
                , do  let n     = mkName ntName
                          nStar = mkName $ ntName ++ "_star"
                      first <- newName "x"
                      rest  <- newName "xs"
                      let me   = varE $ astFncnName $ s ++ "_star"
                          base = varE $ astFncnName s
                      clause [ [p| AST $(conP nStar []) [NT $(conP n []), NT $(conP nStar [])] [ $(varP first), $(varP rest) ] |] ] 
                        (normalB [| ($(base) $(varE first)) : ($(me) $(varE rest)) |]) 
                        []
                , do  param <- newName "param"
                      clause [ [p| $(varP param) |] ]
                        (normalB [| error $ $(litE $ stringL ntName) ++ ": " ++ show $(varE param) |])
                        []
                ])]
            eachAlpha (G4S.GNonTerm (G4S.Regular '+') s) = let -- "_plus"
                ntName = "NT_" ++ s

              in
              [( astFncnName $ s ++ "_plus",
                [ -- First, the "zero or more" base case (returns a singleton list):
                  do  let n     = mkName ntName
                          nPlus = mkName $ ntName ++ "_plus"
                          base  = varE $ astFncnName s
                      param <- newName "param"
                      clause [ [p| AST $(conP nPlus []) [NT $(conP n [])] [$(varP param)] |] ]
                        (normalB [| [$(base) $(varE param)] |])
                        []
                  -- Second, the "zero or more" recursive case (cons the current
                  -- thing onto a recursive call)
                , do  let n     = mkName ntName
                          nPlus = mkName $ ntName ++ "_plus"
                      first <- newName "x"
                      rest  <- newName "xs"
                      let me   = varE $ astFncnName $ s ++ "_plus"
                          base = varE $ astFncnName s
                      clause [ [p| AST $(conP nPlus []) [NT $(conP n []), NT $(conP nPlus [])] [ $(varP first), $(varP rest) ] |] ] 
                        (normalB [| ($(base) $(varE first)) : ($(me) $(varE rest)) |]) 
                        []
                , do  param <- newName "param"
                      clause [ [p| $(varP param) |] ]
                        (normalB [| error $ $(litE $ stringL ntName) ++ ": " ++ show $(varE param) |])
                        []
                ])]
            eachAlpha (G4S.GNonTerm G4S.NoAnnot s) = []
            eachAlpha (G4S.GTerm annot s) = []
          
            mkFncn s = map (\c -> (astFncnName s, [c])) clauses
      
            -- TODO
            makeEpsilonClauses _ = []

          in (concatMap eachAlpha . concatMap G4S.alphas) ps
          --in concatMap makeEpsilonClauses ps
        regex_a2d _ = []
                    
        a2d_error_clauses G4S.Prod{G4S.pName = _A} =
          [(astFncnName _A, [ clause [ [p| ast2 |] ] (normalB [| error (show ast2) |]) [] ])]
        a2d_error_clauses _ = []
          
          --concat $ (concatMap eachAlpha . map G4S.alphas) ps

        epsilon_a2d (G4S.Prod{G4S.pName = _A, G4S.patterns = ps}) = let
          
            mkConP (G4S.GNonTerm annot nt)
              -- Some nonterminals are really terminal tokens (regular expressions):
              | isUpper (head nt)     = conP (mkName "T")  [conP (mkName $ lookupTName "T_" $ annotName annot nt) []]
              | otherwise             = conP (mkName "NT") [conP (mkName $ "NT_" ++ annotName annot nt) []]
            mkConP (G4S.GTerm annot t)   = conP (mkName "T")  [conP (mkName $ lookupTName "T_" $ annotName annot t) []]

            justStr (G4S.GNonTerm annot s) = annotName annot s
            justStr (G4S.GTerm    _     s) = s
            
            justStr' (Left a) = Just $ justStr a
            justStr' _        = Nothing
                
            maybeBaseType (Left _) = Nothing
            maybeBaseType (Right x) = Just x
                
            isValid (Left x)   = G4S.isGNonTerm x
            isValid (Right _)  = True
            --isValid _          = False

            vars :: [Either G4S.ProdElem BaseType] -> [(Maybe BaseType, String, String)]
            vars as = let
                vars' (base_type, i, Just s)   = (base_type, "v" ++ show i ++ "_" ++ s, "ast2" ++ s)
                vars' (Just Mybe, i, Nothing)  = (Just Mybe, "Nothing", "undefined")
                vars' (Just List, i, Nothing)  = (Just List, "[]", "undefined")
                --vars' (base_type, i, Nothing)  = (base_type, "[]", "undefined")
                      

              in (map vars' . map (\(i,a) -> (maybeBaseType a, i, justStr' a)) . filter (isValid . snd) . zip [0 .. length as]) as

            astListPattern as = listP
                  [ case a of
                      (G4S.GNonTerm annot s)  -> varP  $ mkName $ "v" ++ show i ++ "_" ++ annotName annot s
                      otherwise               -> wildP
                  | (i, a) <- catLeftsTuple $ zip [0 .. length as] as
                  ]

            catLeftsTuple :: [(i, Either a b)] -> [(i,a)]
            catLeftsTuple [] = []
            catLeftsTuple ((i, Left x):rst) = (i, x) : catLeftsTuple rst
            catLeftsTuple (_:rst)           = catLeftsTuple rst

            astAppRec b (Just Mybe, varName, _) = appE b (conE $ mkName varName)
            astAppRec b (Just List, varName, _) = appE b (listE [])
            astAppRec b (base_type, varName@(v:_), recName)
              | isLower v = appE b (appE (varE $ mkName recName) $ varE $ mkName varName)
              | otherwise = appE b (appE (varE $ mkName recName) $ conE $ mkName varName)
                {-
                G4S.NoAnnot       -> appE b (appE recName $ varE $ mkName varName)
                (G4S.Regular '?') -> appE b (appE recName $ varE $ mkName varName)
                (G4S.Regular '*') -> appE b (appE recName $ varE $ mkName varName)
                (G4S.Regular '+') -> appE b (appE recName $ varE $ mkName varName)
                otherwise         -> error $ show (b,(varName,recName))
                -}
                
            catLefts [] = []
            catLefts (((Left x)):rst) = x : catLefts rst
            catLefts (_:rst) = catLefts rst

            pats as = [ [p| AST  $(conP (mkName $ "NT_" ++ _A) [])
                                  $(listP $ map mkConP $ catLefts as)
                                  $(astListPattern as)
                         |]
                       ]
            
            appBodyType (base_type, vN@(v:_), rN)
              | isLower v = appE (varE $ mkName rN) $ varE $ mkName vN
              | otherwise = conE $ mkName vN
            
            body dir as = (case (dir, vars as) of
                            (Just (G4S.UpperD d), vs)    -> foldl astAppRec (conE $ mkName d) vs
                            (Just (G4S.LowerD d), vs)    -> foldl astAppRec (varE $ mkName d) vs
                            (Just (G4S.HaskellD d), vs)  -> foldl astAppRec (haskellParseExp d) vs
                            (Nothing, [])   -> tupE []
                            (Nothing, [(Just Mybe, varName, _)]) -> conE $ mkName varName
                            (Nothing, [(Just List, varName, _)]) -> listE []
                            (Nothing, [(base_type, v0@(v:_), rec)])
                              | isUpper v   -> conE $ mkName v0 -- 'Nothing' base case
                              | otherwise   -> appE (varE $ mkName rec) (varE $ mkName v0)
                            (Nothing, vs) -> tupE $ map appBodyType vs
                          )

            e_a2d (G4S.PRHS{G4S.alphas = as0, G4S.pDirective = dir}) = let
               
                isEpsilonAnnot (G4S.Regular '?') = True
                isEpsilonAnnot (G4S.Regular '*') = True
                isEpsilonAnnot _ = False

                combos' :: [Either G4S.ProdElem BaseType] -> [Either G4S.ProdElem BaseType] -> [[Either G4S.ProdElem BaseType]]
                combos' ys [] = []
                combos' ys (a@(Left a'):as)
                  | (isEpsilonAnnot . G4S.annot) a'
                      = (reverse ys ++ (Right $ baseType $ G4S.annot a'):as)  -- Production with epsilon-able alpha 'a' removed
                      : (reverse ys ++ a:as)        -- Production without epsilon-able alpha 'a' removed
                      : (  combos' ((Right $ baseType $ G4S.annot a'):ys) as  -- Recursively with epsilon-able alpha 'a' removed
                        ++ combos' (a:ys) as)       -- Recursively *without* it removed
                  | otherwise = combos' (a:ys) as
                combos' ys ((Right _):as) = error "Can't have 'Right' in second list"

                orderNub ps p1
                  | p1 `elem` ps = ps
                  | otherwise    = p1 : ps

                combos xs = foldl orderNub [] (combos' [] $ map Left xs)
                
              in  [(astFncnName _A,
                    map (\as' -> clause (pats as') (normalB $ body dir as') []) $ combos as0
                  )]

          in concatMap e_a2d ps
        epsilon_a2d _ = []

        allClauses :: [(Name, [ClauseQ])]
        allClauses = (concat . catMaybes . map a2d) ast -- standard clauses ignoring optionals (?,+,*) syntax
                  ++ (concatMap regex_a2d) ast          -- Epsilon-removed optional ast conversion functions
                  ++ (concatMap epsilon_a2d) ast        -- Clauses for productions with epsilons
                  ++ (concatMap a2d_error_clauses) ast  -- Catch-all error clauses

        funDecls lst@((name, _):_) = Just $ funD name $ concatMap snd lst
        funDecls [] = error "groupBy can't return an empty list"

      in (catMaybes . map funDecls . groupBy (\a b -> fst a == fst b) . sortBy (comparing fst)) allClauses

  -- terminaLiterals, lexemeNames

  -- IMPORTANT: Creating type variables in two different haskell type
  -- quasiquoters with the same variable name produces two (uniquely) named type
  -- variables. In order to achieve the same type variable you need to run one
  -- in the Q monad first then pass the resulting type to other parts of the
  -- code that need it (thus capturing the type variable).
  in do 
        
        let tokVal    = mkName "TokenValue"
            tokName   = mkName "TokenName"
            ntSym     = mkName $ ntDataName ast
            tSym      = mkName $ tDataName ast
            nameAST   = mkName (mkUpper $ gName ast ++ "AST")
            nameToken = mkName (mkUpper $ gName ast ++ "Token")
            nameDFAs  = mkName (mkLower $ gName ast ++ "DFAs")
            name      = mkName $ mkLower (gName ast ++ "Grammar'")
            nameUnit  = mkName $ mkLower (gName ast ++ "Grammar")
        prettyTFncnName <- newName "prettifyT"
        prettyValueFncnName <- newName "prettifyValue"
       
        stateTypeName <- newName "s"
        let stateType = varT stateTypeName
        
        let unitTy = [t| () |]
        
        gTyUnit <- justGrammarTy ast unitTy
        gUnitFunD <- funD nameUnit [clause [] (normalB $ [| LL.removeEpsilons $(varE name) |]) []]
        gTySigUnit <- sigD nameUnit (return gTyUnit)

        ntDataDecl <- ntDataDeclQ
        tDataDecl  <- tDataDeclQ
        gTy    <- grammarTy stateType
        gTy'   <- justGrammarTy ast stateType
        gTySig <- sigD name (return gTy)
        g      <- grammar gTy'
        gFunD  <- funD name [clause [] (normalB (return g)) []]
        prettyNT:_     <- [d| instance Prettify $(ntConT ast) where prettify = rshow |]
        prettyT:_      <- [d| instance Prettify $(tConT ast) where prettify = $(varE prettyTFncnName) |]
        prettyValue:_  <- [d| instance Prettify $(conT tokVal) where prettify = $(varE prettyValueFncnName) |]
        lookupTokenD   <- lookupTokenFncnDecl

        tokenNameType  <- tokenNameTypeQ
        tokenValueType <- tokenValueTypeQ
        
        let lName = mkName "l"
        lexeme2Value   <- lexeme2ValueQ lName

        regexes <- mkRegexesQ
        let dfasName    = mkName $ mkLower (gName ast) ++ "DFAs"
        let regexesE    = varE $ mkName $ mkLower (gName ast) ++ "Regexes"
        dfas <- funD dfasName [clause [] (normalB [| map (fst &&& regex2dfa . snd) $(regexesE) |]) []]

        astDecl <-tySynD nameAST   [] [t| AST $(conT ntSym) $(conT nameToken) |]
        tokDecl <- tySynD nameToken [] [t| Token $(conT tSym) $(conT tokVal) |]
       
        decls <- [d|
          instance Ref $(conT ntSym) where
            type Sym $(conT ntSym) = $(conT ntSym)
            getSymbol = id

          tokenize :: String -> [$(conT nameToken)] --Token $(conT tokName) $(conT tokVal)]
          tokenize = T.tokenize $(varE nameDFAs) lexeme2value

          slrParse :: [$(conT nameToken)] -> LR.LRResult (LR.CoreSLRState $(conT ntSym) (StripEOF (Sym $(conT nameToken)))) $(conT nameToken) $(conT nameAST)
          slrParse = (LR.slrParse $(varE nameUnit) event2ast)

          --glrParse :: [$(conT nameToken)] -> LR.LRResult $(conT ntSym) (StripEOF (Sym $(conT nameToken))) $(conT nameToken) $(conT nameAST)
          glrParse :: ($(conT tokName) -> Bool) -> [Char]
                      -> LR.LR1Result
                          --(LR.CoreLR1State $(conT ntSym) (StripEOF (Sym $(conT nameToken))))
                          Int
                          Char
                          $(conT nameAST)
          glrParse filterF = (LR.glrParseInc2 $(varE nameUnit) event2ast (T.tokenizeInc filterF $(varE nameDFAs) lexeme2value))

          instance ALL.Token $(conT nameToken) where
            type Label $(conT nameToken) = StripEOF (Sym $(conT nameToken))
            getLabel = fromJust . stripEOF . getSymbol

            type Literal $(conT nameToken) = $(conT tokVal)
            getLiteral = T.tokenValue

          allstarParse :: [$(conT nameToken)] -> Either String $(conT nameAST)
          allstarParse inp = ALL.parse inp (ALL.NT $(s0)) (ALL.atnOf ($(varE nameUnit) :: $(justGrammarTy ast unitTy))) True

          the_ast = $(lift ast)
          |]
        
        prettyTFncn <- prettyTFncnQ prettyTFncnName
        prettyVFncn <- prettyVFncnQ prettyValueFncnName

        ast2DTFncns <- sequence $ ast2DTFncnsQ nameAST

        return $
          [ ntDataDecl, tDataDecl
          , gTySig,     gFunD
          , gTySigUnit, gUnitFunD
          , tokenNameType, tokenValueType
          , prettyTFncn, prettyVFncn
          , prettyNT, prettyT, prettyValue
          , lookupTokenD
          , lexeme2Value
          , regexes
          , dfas, astDecl, tokDecl
          ] ++ decls ++ ast2DTFncns

mkLRParser ast g = 
  let
    nameDFAs  = mkName (mkLower $ gName ast ++ "DFAs")
    tokName   = mkName "TokenName"
    nameAST   = mkName (mkUpper $ grammarName ast ++ "AST")
    name = mkName $ mkLower (grammarName ast ++ "Grammar")
    is = sort $ S.toList $ LR.lr1Items g
    lr1Table' = LR.convTableInt (LR.lr1Table g) is
    lr1S0'    = LR.convStateInt is $ LR.lr1Closure g $ LR.lr1S0 g

    unitTy = [t| () |]
    name' = [e| $(varE name) |] -- :: $(justGrammarTy' ast unitTy) |]
  in do {-D.traceM $ pshow' is
        D.traceM $ "lr1S0 = " ++ (pshow' $ LR.lr1S0 g)
        D.traceM $ "lr1Table = " ++ (pshow' $ LR.lr1Table g)
        D.traceM $ "lr1S0' = " ++ (pshow' lr1S0')
        D.traceM $ "lr1Table' = " ++ (pshow' lr1Table') -}
          --
          --glrParse filterF = (LR.glrParseInc2 $(varE nameUnit) event2ast (T.tokenizeInc filterF $(varE nameDFAs) lexeme2value))
        
        [d| lr1ItemsList = sort $ S.toList $ LR.lr1Items $(name')
            lr1Table    = $(lift lr1Table')
            lr1Goto     = LR.convGotoStatesInt (LR.convGoto $(name') (LR.lr1Goto $(name')) lr1ItemsList) lr1ItemsList
            lr1Closure  = convState $ LR.lr1Closure $(name') (LR.lr1S0 $(name'))
            lr1S0       = $(lift lr1S0')
            convState   = LR.convStateInt lr1ItemsList

            glrParseFast :: ($(conT tokName) -> Bool) -> [Char]
                        -> LR.LR1Result
                            --(LR.CoreLR1State $(conT ntSym) (StripEOF (Sym $(conT nameToken))))
                            Int
                            Char
                            $(conT nameAST)
            glrParseFast filterF =
              LR.glrParseInc'
                $(name')
                lr1Table
                lr1Goto
                lr1S0
                (LR.tokenizerFirstSets convState $(name'))
                event2ast
                (T.tokenizeInc filterF $(varE nameDFAs) lexeme2value)
            |]

