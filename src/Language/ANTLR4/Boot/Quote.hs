{-# LANGUAGE  QuasiQuotes, TemplateHaskell, ScopedTypeVariables, DataKinds,
              LambdaCase, FlexibleContexts #-}
{-|
  Module      : Language.ANTLR4.Boot.Quote
  Description : ANTLR4 boot-level quasiquoter
  Copyright   : (c) Karl Cronburg, 2018
  License     : BSD3
  Maintainer  : karl@cs.tufts.edu
  Stability   : experimental
  Portability : POSIX
-}
module Language.ANTLR4.Boot.Quote
  ( antlr4
  , g4_decls
  , g4_parsers
  , mkLRParser
  ) where
import Prelude hiding (exp, init)
import System.IO.Unsafe (unsafePerformIO)
import Data.List (nub, elemIndex, groupBy, sortBy, sort, intersperse)
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

import Text.ANTLR.Grammar hiding (getNTs, getProds, s0)
import Text.ANTLR.Parser (AST(..), StripEOF(..))
import Text.ANTLR.Pretty
import Text.ANTLR.Lex.Tokenizer as T
import Text.ANTLR.LR as LR
import qualified Text.ANTLR.Allstar as ALL
import qualified Text.ANTLR.LL1 as LL
import qualified Text.ANTLR.Set as S

import qualified Text.ANTLR.MultiMap as M
import qualified Data.Map as M1
import Text.ANTLR.Set (Set(..))
import qualified Text.ANTLR.Set as Set
import qualified Text.ANTLR.Lex.Regex as R

--trace s = D.trace   ("[Language.ANTLR4.Boot.Quote] " ++ s)
--traceM s = D.traceM ("[Language.ANTLR4.Boot.Quote] " ++ s)

trace s x = x
traceM s x = x

haskellParseExp :: (Monad m) => String -> m TH.Exp
haskellParseExp s = --D.trace ("PARSING: " ++ show s) $
  case LHM.parseExp s of
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

-- | There are three different quasiquoters in antlr-haskell, each with varying
--   support for different G4 features. If you're looking for the user-facing
--   quasiquoter then turn back now, because here-be-dragons. The user-facing
--   quasiquoter can be found in 'Language.ANTLR4.G4' as @g4@.
--
--   * __User-facing__ QuasiQuoter is in 'Language.ANTLR4.G4'
--   * __Spliced__ QuasiQuoter is here
--   * __Boot__ parser is in @src/Language/ANTLR4/Boot/Parser.hs.boot@
--
--   The spliced quasiquoter, as packaged and shipped with distributions of
--   antlr-haskell, allows for bootstrapping of the user-facing quasiquoter
--   without requiring parsec as a dependency. The boot quasiquoter on the
--   other hand is written entirely in parsec.
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

justGrammarTy ast s = [t| Grammar $(s) $(ntConT ast) $(tConT ast) G4S.Directive |]
justGrammarTy' ast s = [t| Grammar $(s) $(ntConT ast) (StripEOF (Sym $(tConT ast))) G4S.Directive |]

ntConT ast = conT $ mkName $ ntDataName ast
tConT  ast = conT $ mkName $ tDataName ast

ntDataName ast = gName ast ++ "NTSymbol"
tDataName  ast = gName ast ++ "TSymbol"

gName ast = grammarName ast

type G4AST = [G4S.G4]

-- A list of all the G4 literal terminals scattered across production rules
terminalLiterals :: G4AST -> [String]
terminalLiterals ast = (nub $ concatMap getTerminals ast)
    
-- Find all terminal literals in a G4 grammar rule like '(' and ')' and ';'
getTerminals :: G4S.G4 -> [String]
getTerminals G4S.Prod{G4S.patterns = ps} = concatMap (justTerms . G4S.alphas) ps
getTerminals _ = []

-- THIS EXCLUDES LEXEME FRAGMENTS:
-- e.g. [('UpperID', AString), ('SetChar', Named String)]
lexemeTypes :: G4AST -> [(String, LexemeType)]
lexemeTypes ast = let

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

-- A list of all the G4 lexeme names specified in the grammar
lexemeNames :: G4AST -> [String]
lexemeNames ast = map fst (lexemeTypes ast)
    
-- Find all terminals *literals* in a production like '(' and ')' and ';'
justTerms :: [G4S.ProdElem] -> [String]
justTerms [] = []
justTerms ((G4S.GTerm _ s) : as) = s : justTerms as
justTerms (_:as) = justTerms as

-- A list of all the terminals in the grammar (both literal G4 terminals and
-- G4 lexical terminals)
terminals :: G4AST -> [String]
terminals ast = terminalLiterals ast ++ (lexemeNames ast)

nonterms :: G4AST -> [String]
nonterms ast = nub $ concatMap getNTs ast

-- Find all nonterminals in a production like 'exp' and 'decl'
justNonTerms :: [G4S.ProdElem] -> [String]
justNonTerms [] = []
justNonTerms (G4S.GNonTerm _ s:as)
  | (not . null) s && isLower (head s) = s : justNonTerms as
  | otherwise = justNonTerms as
justNonTerms (_:as) = justNonTerms as

-- Find all the nonterminals referenced in the production(s) of the given grammar rule
getNTs :: G4S.G4 -> [String]
getNTs G4S.Prod{G4S.pName = pName, G4S.patterns = ps} = pName : concatMap (justNonTerms . G4S.alphas) ps
getNTs _ = []

-- Things Symbols must derive:
symbolDerives = derivClause Nothing $ map (conT . mkName)
  [ "Eq", "Ord", "Show", "Hashable", "Generic", "Bounded", "Enum", "Data", "Lift"]

-- Nonterminal symbol data type (enum) for this grammar:
ntDataDeclQ :: G4AST -> DecQ
ntDataDeclQ ast =
  dataD (cxt [])
  (mkName $ ntDataName ast)
  []
  Nothing
  (map (\s -> normalC (mkName $ "NT_" ++ s) []) $ (nonterms ast) ++ (regexNonTermSymbols ast))
  [symbolDerives]

-- E.g. ['(', ')', ';', 'exp', 'decl']
allLexicalSymbols :: G4AST -> [String]
allLexicalSymbols ast = map (lookupTName ast "") (terminalLiterals ast) ++ (lexemeNames ast)

-- E.g. [('(', Literal 0), (')', Literal 1), (';', Literal 2), ('exp',
-- AString), ('decl', AString')]
allLexicalTypes :: G4AST -> [(String, LexemeType)]
allLexicalTypes ast = (map (lookupLiteralType ast) (terminalLiterals ast)) ++ (lexemeTypes ast)

-- E.g. [('(', Literal 0), ...]
lookupLiteralType :: G4AST -> String -> (String, LexemeType)
lookupLiteralType ast s =
  case s `elemIndex` (terminalLiterals ast) of
    Nothing -> undefined
    Just i  -> (s, Literal i)

-- Terminal symbol data type (enum) for this grammar:
tDataDeclQ :: G4AST -> DecQ
tDataDeclQ ast =
  dataD (cxt [])
    (mkName $ tDataName ast)
    []
    Nothing
    (map (\s -> normalC (mkName s) []) (map ("T_" ++) (allLexicalSymbols ast)))
    --(\s -> normalC (mkName $ lookupTName ast "T_" s) []) lexemes) ++ (lexemeNames "T_"))
    [symbolDerives]

-- Map from a terminal's syntax to the name of the data type instance from
-- tDataDeclQ:
lookupTName :: G4AST -> String -> String -> String
lookupTName ast pfx s = pfx ++
  (case s `elemIndex` (terminalLiterals ast) of
    Nothing -> s
    Just i  -> show i)

defBang = bang noSourceUnpackedness noSourceStrictness

strBangType = (defBang, conT $ mkName "String")

mkCon   = conE . mkName . mkUpper
mkConNT = conE . mkName . ("NT_" ++)

-- | Regular expression term annotations are just syntactic sugar by any other name.
--   Computes the set of productions that need to be added to the grammar to support
--   surface-syntax-level annotations on production rule terms.
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
      [ withAlphas (nt ++ "_quest") (G4S.UpperD "Just") [G4S.GNonTerm G4S.NoAnnot nt]
      , withAlphas (nt ++ "_quest") (G4S.UpperD "Nothing") [] -- epsilon
      ]
    gTAP (G4S.GNonTerm (G4S.Regular '*') nt) =
      [ withAlphas (nt ++ "_star")  (G4S.HaskellD "(:)")  [G4S.GNonTerm G4S.NoAnnot nt, G4S.GNonTerm G4S.NoAnnot (nt ++ "_star")]
      , withAlphas (nt ++ "_star")  (G4S.HaskellD "(\\x -> [x])")  [G4S.GNonTerm G4S.NoAnnot nt]
      , withAlphas (nt ++ "_star")  (G4S.HaskellD "[]")  []
      ]
    gTAP (G4S.GNonTerm (G4S.Regular '+') nt) =
      [ withAlphas (nt ++ "_plus")  (G4S.HaskellD "(:)")  [G4S.GNonTerm G4S.NoAnnot nt, G4S.GNonTerm G4S.NoAnnot (nt ++ "_plus")]
      , withAlphas (nt ++ "_plus")  (G4S.HaskellD "(\\x -> [x])")  [G4S.GNonTerm G4S.NoAnnot nt]
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

regexNonTermSymbols ast = let

    rNTS (G4S.Prod {G4S.patterns = ps}) = Just $ map G4S.alphas ps
    rNTS _ = Nothing

  in nub $ map annotName' $ filter (not . G4S.isNoAnnot . G4S.annot) (concat $ concat $ catMaybes $ map rNTS ast)

{-
toElem :: G4AST -> G4S.ProdElem, Maybe DataType) -> (TH.ExpQ
toElem ast (G4S.GTerm annot s, dt)    = ([| $(mkCon "T")  $(mkCon $ lookupTName ast "T_" (annotName annot s)) |], dt)
toElem ast (G4S.GNonTerm annot s, dt)
  | (not . null) s && isLower (head s) = ([| $(mkCon "NT") $(mkConNT (annotName annot s)) |], dt)
  | otherwise = toElem ast (G4S.GTerm G4S.NoAnnot s, dt)

mkProd :: String -> [(TH.ExpQ, Maybe DataType)] -> TH.ExpQ
mkProd n [] = [| $(mkCon "Production") $(conE $ mkName $ "NT_" ++ n) ($(mkCon "Prod") $(mkCon "Pass") [Eps]) (Just "") |]
mkProd n es = [| $(mkCon "Production") $(conE $ mkName $ "NT_" ++ n) ($(mkCon "Prod") $(mkCon "Pass") $(listE es)) (Just "") |]

getProds :: G4AST -> [G4S.G4] -> [TH.ExpQ]
getProds ast [] = []
getProds ast (G4S.Prod {G4S.pName = n, G4S.patterns = ps}:xs)
  = map (mkProd n . map (toElem ast) . (\p -> (G4S.alphas p, G4S.pDirective p))) ps ++ ((getProds ast) xs)
getProds ast (_:xs) = (getProds ast) xs
-}

getProds :: G4AST -> [G4S.G4] -> [TH.ExpQ]
getProds ast [] = []
getProds ast (G4S.Prod {G4S.pName = n, G4S.patterns = ps}:xs) = let

    toElem :: G4S.ProdElem -> TH.ExpQ
    toElem (G4S.GTerm annot s)    = [| $(mkCon "T")  $(mkCon $ lookupTName ast "T_" (annotName annot s)) |]
    toElem (G4S.GNonTerm annot s)
      | (not . null) s && isLower (head s) = [| $(mkCon "NT") $(mkConNT (annotName annot s)) |]
      | otherwise = toElem (G4S.GTerm G4S.NoAnnot s)

    mkProd :: Maybe G4S.Directive -> [TH.ExpQ] -> TH.ExpQ
    mkProd dir [] = [| $(mkCon "Production") $(conE $ mkName $ "NT_" ++ n) ($(mkCon "Prod") $(mkCon "Pass") []) $(lift dir) |]
    mkProd dir es = [| $(mkCon "Production") $(conE $ mkName $ "NT_" ++ n) ($(mkCon "Prod") $(mkCon "Pass") $(listE es)) $(lift dir) |]

  in map (\p -> mkProd (G4S.pDirective p) (map toElem $ G4S.alphas p)) ps ++ ((getProds ast) xs)
getProds ast (_:xs) = (getProds ast) xs

-- The first NonTerminal in the grammar (TODO: head of list)
s0 :: G4AST -> TH.ExpQ
s0 ast = conE $ mkName $ "NT_" ++ head (nonterms ast)

grammarProds ast = getProds ast (ast ++ (D.traceShowId (genTermAnnotProds ast)))

grammar ast gTy = [| (defaultGrammar $(s0 ast) :: $(return gTy))
  { ns = Set.fromList [minBound .. maxBound :: $(ntConT ast)]
  , ts = Set.fromList [minBound .. maxBound :: $(tConT ast)]
  , ps = $(listE $ grammarProds ast)
  } |]

--grammarTy s = [t| forall $(s). (Prettify $(s)) => $(justGrammarTy s) |]
grammarTy ast s = [t| (Prettify $(s)) => $(justGrammarTy ast s) |]

{----------------------- Tokenizer -----------------------}

tokenNameTypeQ ast = tySynD (mkName "TokenName") [] (conT $ mkName $ tDataName ast)

lexemeValueDerives = derivClause Nothing $ map (conT . mkName)
  ["Show", "Ord", "Eq", "Generic", "Hashable", "Data"]

--
lexemeTypeConstructors ast = let
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
    ++ (map (\s -> normalC (mkName $ lookupTName ast "V_" s) []) (terminalLiterals ast)))

tokenValueTypeQ ast =
  dataD (cxt []) (mkName "TokenValue") [] Nothing
  (lexemeTypeConstructors ast)
  [lexemeValueDerives]

mkTyVar s f = return $ f $ mkName s

lookupTokenFncnDecl ast = let
    lTFD t = clause [litP $ stringL t]
              (normalB $ [| Token   $(conE $ mkName   $ lookupTName ast "T_" t)
                                    $(conE $ mkName   $ lookupTName ast "V_" t)
                                    $(litE $ integerL $ fromIntegral $ length t) |])
              []
  in funD (mkName "lookupToken")
    (  map lTFD (terminalLiterals ast)
    ++ [clause [varP $ mkName "s"]
        (normalB $ [| error ("Error: '" ++ s ++ "' is not a token") |])
        []]
    )

-- Construct the function that takes in a lexeme (string) and the token name
-- (T_*) and constructs a token value type instance using 'read' where
-- appropriate based on the directives given in the grammar.
lexeme2ValueQ ast lName = let

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

  in funD (mkName "lexeme2value") (map l2VQ (allLexicalTypes ast))

-- Convert a G4 regex into the backend regex type (for constructing token
-- recognizers as DFAs):
convertRegex :: (Show c) => (String -> G4S.Regex c) -> G4S.Regex c -> R.Regex c
convertRegex getNamedR = let
    cR G4S.Epsilon       = R.Epsilon
    cR (G4S.Literal [])  = R.Epsilon
    cR (G4S.Literal [c]) = R.Symbol c
    cR (G4S.Literal cs)  = R.Literal cs
    cR (G4S.Union rs)    = R.MultiUnion $ map cR rs
    cR (G4S.Concat rs)   = R.Concat $ map cR rs
    cR (G4S.Kleene r)    = R.Kleene $ cR r
    cR (G4S.PosClos r)   = R.PosClos $ cR r
    cR (G4S.Question r)  = R.Question $ cR r
    cR (G4S.CharSet cs)  = R.Class cs
    cR (G4S.Named n)     = convertRegex getNamedR $ getNamedR n
    cR (G4S.Negation (G4S.CharSet cs)) = R.NotClass cs
    cR (G4S.Negation (G4S.Literal s)) = R.NotClass s
    cR (G4S.Negation (G4S.Concat [G4S.Literal s])) = R.NotClass s
    cR r@(G4S.Negation _) = error $ "unimplemented: " ++ show r
  in cR

getNamedRegex :: G4AST -> String -> G4S.Regex Char
getNamedRegex ast n = let
    -- Only the lexeme (fragments) with the given name:
    gNR (G4S.Lex{G4S.annotation = Just G4S.Fragment, G4S.lName = lName}) = lName == n
    gNR _ = False
  in case filter gNR ast of
        [] -> error $ "No fragment named '" ++ n ++ "'"
        [(G4S.Lex{G4S.pattern = G4S.LRHS{G4S.regex = r}})] -> r
        xs -> error $ "Too many fragments named '" ++ n ++ "', i.e.: " ++ show xs

-- Make the list of tuples containing regexes, one for each terminal.
mkRegexesQ ast = let
    mkLitR :: String -> ExpQ
    mkLitR s = [| ($( conE $ mkName $ lookupTName ast "T_" s)
                    , $(lift $ convertRegex (getNamedRegex ast) $ G4S.Literal s)) |]

    mkLexR :: G4S.G4 -> Maybe ExpQ
    mkLexR (G4S.Lex{G4S.annotation = Nothing, G4S.lName = lName, G4S.pattern = G4S.LRHS{G4S.regex = r}}) = Just
      [| ($(conE $ mkName $ lookupTName ast "T_" lName), $(lift $ convertRegex (getNamedRegex ast) r)) |]
    mkLexR _ = Nothing
  in valD (varP $ mkName $ mkLower $ gName ast ++ "Regexes")
      (normalB $ listE (map mkLitR (terminalLiterals ast) ++ (catMaybes $ map mkLexR ast)))
      []

prettyTFncnQ ast fncnName = let
    pTFLit lexeme =
      clause [conP (mkName $ lookupTName ast "T_" lexeme) []]
      (normalB [| pStr $(litE $ stringL $ "'" ++ lexeme ++ "'") |])
      []

    pTFName lexeme =
      clause [conP (mkName $ lookupTName ast "T_" lexeme) []]
      (normalB [| pStr $(litE $ stringL $ lexeme) |])
      []
  in funD fncnName (map pTFLit (terminalLiterals ast) ++ map pTFName (lexemeNames ast))

prettyVFncnQ ast fncnName = let
    pVFLit lexeme =
      clause [conP (mkName $ lookupTName ast "V_" lexeme) []]
      (normalB [| pStr $(litE $ stringL $ "'" ++ lexeme ++ "'") |])
      []

    pVFName lexeme =
      clause [conP (mkName $ lookupTName ast "V_" lexeme) [varP (mkName "v")]]
      (normalB [| pChr '\'' >> prettify v >> pChr '\'' |])
      []
  in funD fncnName (map pVFLit (terminalLiterals ast) ++ map pVFName (lexemeNames ast))

astFncnName s = mkName $ "ast2" ++ s

a2d ast nameAST G4S.Lex{G4S.annotation = Nothing, G4S.lName  = _A, G4S.pattern = G4S.LRHS{G4S.directive = dir}}
  = Just [(mkName $ "ast2" ++ _A
           ,[ clause  [ conP (mkName "Leaf")
                        [ conP (mkName $ "Token")
                          [ wildP
                          , conP (mkName $ lookupTName ast "V_" _A)
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
a2d ast nameAST G4S.Prod{G4S.pName = _A, G4S.patterns = ps} = let

  mkConP (G4S.GNonTerm annot nt)
    -- Some nonterminals are really terminal tokens (regular expressions):
    | isUpper (head nt)     = conP (mkName "T")  [conP (mkName $ lookupTName ast "T_" $ annotName annot nt) []]
    | otherwise             = conP (mkName "NT") [conP (mkName $ "NT_" ++ annotName annot nt) []]
  mkConP (G4S.GTerm annot t)   = conP (mkName "T")  [conP (mkName $ lookupTName ast "T_" $ annotName annot t) []]

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

  astAppRec b (alpha, varName, recName) = appE b (appE recName $ varE varName)
  {- case G4S.annot alpha of
      G4S.NoAnnot       -> appE b (appE recName $ varE varName)
      (G4S.Regular '?') -> appE b (appE recName $ varE varName)
      -- TODO: Below two cases:
      (G4S.Regular '*') -> appE b (appE recName $ varE varName)
      (G4S.Regular '+') -> appE b (appE recName $ varE varName)
      otherwise         -> error $ show alpha -}

  clauses = [ clause  [ [p| AST $(conP (mkName $ "NT_" ++ _A) [])
                             $(listP $ map mkConP as)
                             $(astListPattern as)
                        |]
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
a2d ast nameAST _ = Nothing

a2d_error_clauses G4S.Prod{G4S.pName = _A} =
  [(astFncnName _A, [ clause [ [p| ast2 |] ] (normalB [| error (show ast2) |]) [] ])]
a2d_error_clauses _ = []

  --concat $ (concatMap eachAlpha . map G4S.alphas) ps

{-
epsilon_a2d ast (G4S.Prod{G4S.pName = _A, G4S.patterns = ps}) = let

    mkConP (G4S.GNonTerm annot nt)
      -- Some nonterminals are really terminal tokens (regular expressions):
      | isUpper (head nt)     = conP (mkName "T")  [conP (mkName $ lookupTName ast "T_" $ annotName annot nt) []]
      | otherwise             = conP (mkName "NT") [conP (mkName $ "NT_" ++ annotName annot nt) []]
    mkConP (G4S.GTerm annot t)   = conP (mkName "T")  [conP (mkName $ lookupTName ast "T_" $ annotName annot t) []]

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

    pats as =  [ [p| AST  $(conP (mkName $ "NT_" ++ _A) [])
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
epsilon_a2d ast _ = []
-}

mkTupler n = let
    xs = ["p" ++ show i | i <- [0 .. n - 1]]
    xs_comma = intersperse "," xs
  in "(\\" ++ concat (intersperse " " xs) ++ " -> (" ++ concat xs_comma ++ "))"

-- | Post-condition: all TermAnnots in this production are NoAnnots,
--   and all directives are not Nothing (Nothings turn into Unit, identity function, or tupler).
wipeOutAnnots p@(G4S.Prod{G4S.pName = _A, G4S.patterns = ps}) = let

    wOA prhs@(G4S.PRHS { G4S.alphas = as0, G4S.pDirective = dir }) = let
        
        repAnnots pe@(G4S.GTerm G4S.NoAnnot _) = pe
        repAnnots pe@(G4S.GNonTerm G4S.NoAnnot _) = pe
        repAnnots (G4S.GTerm a s) = G4S.GTerm G4S.NoAnnot (annotName a s)
        repAnnots (G4S.GNonTerm a s) = G4S.GNonTerm G4S.NoAnnot (annotName a s)

        dir' = let
            as0' = filter G4S.isGNonTerm as0
          in case dir of
            Just x  -> Just x
            Nothing
              | length as0' == 0 -> Just $ G4S.HaskellD "()"
              | length as0' == 1 -> Just $ G4S.HaskellD "(\\x -> x)"
              | otherwise       -> Just $ G4S.HaskellD $ mkTupler (length as0')

      in prhs { G4S.alphas = map repAnnots as0, G4S.pDirective = dir' }

  in p { G4S.patterns = map wOA ps }
wipeOutAnnots x = x

--allClauses :: Grammar s nts t -> G4AST -> [(Name, [ClauseQ])]
allClauses gr ast' nameAST = let

    ast = genTermAnnotProds ast' ++ ast'

  in 
             (concat . catMaybes . map (a2d ast nameAST)) ast -- standard clauses ignoring optionals (?,+,*) syntax
{-          ++ (concatMap regex_a2d) ast        -- Epsilon-removed optional ast conversion functions -}
{-          ++ (concatMap (epsilon_a2d ast)) ast  -- Clauses for productions with epsilons -}
          ++ (concatMap a2d_error_clauses) ast  -- Catch-all error clauses

funDecls lst@((name, _):_) = Just $ funD name $ concatMap snd lst
funDecls [] = error "groupBy can't return an empty list"

-- Pattern matches on an AST to produce a Maybe DataType
ast2DTFncnsQ gr ast nameAST =
  (catMaybes . map funDecls . groupBy (\a b -> fst a == fst b) . sortBy (comparing fst)) (allClauses gr ast nameAST)

unitTy = [t| () |]

removeEpsilonsAST :: [G4S.G4] -> [G4S.G4]
removeEpsilonsAST ast = let
    
    getPRHS (G4S.Prod { G4S.pName = s, G4S.patterns = ps }) = map (\as -> (s, as)) ps
    getPRHS _ = []

    epsNT (_A, G4S.PRHS { G4S.alphas = [], G4S.pDirective = dir}) = (:) (_A, dir)
    epsNT _ = id

    epsNTs = foldr epsNT [] (concatMap getPRHS ast)

    -- Maintains order with a foldr
    orderNub ast0 asts
      | ast0 `elem` asts = asts
      | otherwise        = ast0 : asts

    replicateDeclFor (nts0, dflt) (G4S.Prod { G4S.pName = nt1, G4S.patterns = ps }) = let

        -- Reconstruct the directive such that we drop one symbol (NT or T) between ys xs
        -- (starting with ys, ending with xs)
        dropOne ys' xs' dir =
          let ys = filter G4S.isGNonTerm ys'
              xs = filter G4S.isGNonTerm xs'
          
              params_ys = map (\i -> " p" ++ show i ++ " ") [0 .. length ys - 1]
              params_xs = map (\i -> " p" ++ show i ++ " ") [length ys .. length ys + length xs - 1]
              
              both = concat (intersperse "," $ params_ys ++ params_xs)

              ifNull s
                | null s    = "id"
                | otherwise = s

              s_dir = case dir of
                Just (G4S.UpperD s)     -> "(" ++ ifNull s ++ ")"
                Just (G4S.LowerD s)     -> "(" ++ ifNull s ++ ")"
                Just (G4S.HaskellD s)   -> "(" ++ ifNull s ++ ")"
                -- tuple-er:
                Nothing
                  | length (params_ys ++ params_xs) == 0 -> "()"
                  | length (params_ys ++ params_xs) == 1 -> "(\\x -> x)"
                  | otherwise -> "(\\" ++ concat params_ys ++ concat params_xs ++ " -> ("
                                  ++  both ++ "))"
              
              s_dflt = case dflt of
                Just (G4S.UpperD s) -> s
                Just (G4S.LowerD s) -> s
                Just (G4S.HaskellD s) -> s
                Nothing -> "    ()    "

              ret
                | length params_ys + length params_xs == 0 = Just $ G4S.HaskellD $ "(" ++ s_dir ++ " " ++ s_dflt ++ ")"
                | otherwise = Just $ G4S.HaskellD $ "(\\" ++ concat params_ys ++ concat params_xs ++ " -> " ++ s_dir
                                ++ " " ++ concat params_ys ++ " " ++ s_dflt ++ " " ++ concat params_xs ++ ")"
            
            in ret
 
        rDF prhs ys [] = [ updatePRHS prhs $ reverse ys ]
        rDF prhs ys (x:xs) = let

          newPRHS = prhs { G4S.pDirective = dropOne ys xs (G4S.pDirective prhs) }

          result
            | G4S.prodElemSymbol x == nts0 -- String equality
                = updatePRHS newPRHS (reverse ys ++ xs)
                : updatePRHS prhs    (reverse ys ++ x:xs)
                : (  rDF newPRHS ys     xs  -- Recursively with nts0 removed
                  ++ rDF prhs    (x:ys) xs) -- Recursively without nts0 removed
            | otherwise = rDF prhs (x:ys) xs
       
          in result

        updatePRHS prhs xs = prhs { G4S.alphas = xs }

      in  ( G4S.Prod
             { G4S.pName    = nt1
             -- TODO: nub by ignoring directives? Really the directives need to be types not strings...
             , G4S.patterns = nub $ concatMap 
                              (\prhs -> rDF prhs [] (G4S.alphas prhs))
                              ps
             }
          )
    replicateDeclFor _ p = p

    eliminate nts prod@(G4S.Prod { G4S.pName = _A, G4S.patterns = ps }) =
      if _A == nts
        then prod { G4S.patterns = filter (not . null . G4S.alphas) ps }
        else prod
    eliminate nts prod = prod

    ast' = case D.trace ("epsNTs: " ++ show epsNTs) epsNTs of
      [] -> ast
      ((nts, dflt):ntss) -> removeEpsilonsAST $
        map (eliminate nts) (foldr orderNub [] (map (replicateDeclFor (nts, dflt)) ast))

  in foldr orderNub [] ast'

{-
    epsNT (_A, G4S.PRHS { G4S.alphas = [] }) = (:) _A
    epsNT prod                               = id

    ps_init = concatMap (\

    epsNTs = foldr epsNT [] (map (second (filter (not . isEps))) ps_init)

    orderNub ps p1
      | p1 `elem` ps = ps
      | otherwise    = p1 : ps

    replicateProd nts0 (nt1, es) = let
      
        rP ys [] = [(nt1, reverse ys)]
        rP

      in rP [] es

    ps' = case epsNTs of
      []          -> ps_init
      (nts:ntss)  -> removeEpsilonsAST $
                      foldl orderNub [
                            [ p'
                            | (_A, as) <- ps_init
                            , p' <- replicateProd nts (_A, as)
                            , (not . null) as
                            ]


  in ps'
-}

-- | This function does the heavy-lifting of Haskell code generation, most notably
--   generating non-terminal, terminal, and grammar data types as well as accompanying
--   parsing functions.
g4_decls :: [G4S.G4] -> TH.Q [TH.Dec] -- exp :: G4
g4_decls ast' =
  -- terminaLiterals, lexemeNames

  -- IMPORTANT: Creating type variables in two different haskell type
  -- quasiquoters with the same variable name produces two (uniquely) named type
  -- variables. In order to achieve the same type variable you need to run one
  -- in the Q monad first then pass the resulting type to other parts of the
  -- code that need it (thus capturing the type variable).
  do  let ast       = removeEpsilonsAST $ map wipeOutAnnots (ast' ++ genTermAnnotProds ast') -- Order of '++' matters here

          tokVal    = mkName "TokenValue"
          tokName   = mkName "TokenName"
          ntSym     = mkName $ ntDataName ast
          tSym      = mkName $ tDataName ast
          nameAST   = mkName (mkUpper $ gName ast ++ "AST")
          nameToken = mkName (mkUpper $ gName ast ++ "Token")
          nameDFAs  = mkName (mkLower $ gName ast ++ "DFAs")
          name      = mkName $ mkLower (gName ast ++ "Grammar'")
          nameUnit  = mkName $ mkLower (gName ast ++ "Grammar")
          lowerASTName = mkName (mkLower $ gName ast ++ "AST")
      
      D.traceM $ "AST=" ++ pshowList' ast

      prettyTFncnName <- newName "prettifyT"
      prettyValueFncnName <- newName "prettifyValue"

      stateTypeName <- newName "s"
      let stateType = varT stateTypeName

      gTyUnit <- justGrammarTy ast unitTy
      --gUnitFunD <- funD nameUnit [clause [] (normalB $ [| LL.removeEpsilons $(varE name) |]) []]
      gUnitFunD <- funD nameUnit [clause [] (normalB $ [| $(varE name) |]) []]
      gTySigUnit <- sigD nameUnit (return gTyUnit)

      ntDataDecl <- ntDataDeclQ ast
      tDataDecl  <- tDataDeclQ ast
      gTy    <- grammarTy ast stateType
      gTy'   <- justGrammarTy ast stateType
      gTySig <- sigD name (return gTy)
      g      <- grammar ast gTy'
      gFunD  <- funD name [clause [] (normalB (return g)) []]
      prettyNT:_     <- [d| instance Prettify $(ntConT ast) where prettify = rshow |]
      prettyT:_      <- [d| instance Prettify $(tConT ast) where prettify = $(varE prettyTFncnName) |]
      prettyValue:_  <- [d| instance Prettify $(conT tokVal) where prettify = $(varE prettyValueFncnName) |]
      lookupTokenD   <- lookupTokenFncnDecl ast

      tokenNameType  <- tokenNameTypeQ ast
      tokenValueType <- tokenValueTypeQ ast

      let lName = mkName "l"
      lexeme2Value   <- lexeme2ValueQ ast lName

      regexes <- mkRegexesQ ast
      let dfasName    = mkName $ mkLower (gName ast) ++ "DFAs"
      let regexesE    = varE $ mkName $ mkLower (gName ast) ++ "Regexes"
      dfas <- funD dfasName [clause [] (normalB [| map (fst &&& regex2dfa . snd) $(regexesE) |]) []]

      astDecl <-tySynD nameAST   [] [t| AST $(conT ntSym) $(conT nameToken) |]
      tokDecl <- tySynD nameToken [] [t| Token $(conT tSym) $(conT tokVal) |]

      prettyTFncn <- prettyTFncnQ ast prettyTFncnName
      prettyVFncn <- prettyVFncnQ ast prettyValueFncnName
      
      the_ast <- funD lowerASTName [clause [] (normalB $ lift ast) []] -- [d| $(lowerASTName) = $(lift ast) |]

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
        , the_ast
        ]

g4_parsers ast gr = do
  let tokVal    = mkName "TokenValue"
      tokName   = mkName "TokenName"
      ntSym     = mkName $ ntDataName ast
      tSym      = mkName $ tDataName ast
      nameAST   = mkName (mkUpper $ gName ast ++ "AST")
      nameToken = mkName (mkUpper $ gName ast ++ "Token")
      nameDFAs  = mkName (mkLower $ gName ast ++ "DFAs")
      name      = mkName $ mkLower (gName ast ++ "Grammar'")
      nameUnit  = mkName $ mkLower (gName ast ++ "Grammar")
  
  D.traceM $ "This is the grammar: " ++ pshow' gr
  ast2DTFncns <- sequence $ ast2DTFncnsQ gr ast nameAST
  decls <- [d|
      instance Ref $(conT ntSym) where
        type Sym $(conT ntSym) = $(conT ntSym)
        getSymbol = id

      tokenize :: String -> [$(conT nameToken)] --Token $(conT tokName) $(conT tokVal)]
      tokenize = T.tokenize $(varE nameDFAs) lexeme2value

      slrParse :: [$(conT nameToken)]
                  -> LR.LRResult
                    (LR.CoreSLRState $(conT ntSym) (StripEOF (Sym $(conT nameToken))))
                    $(conT nameToken)
                    $(conT nameToken)
                    $(conT nameAST)
      slrParse = (LR.slrParse $(varE nameUnit) event2ast)

      --glrParse :: [$(conT nameToken)] -> LR.LRResult $(conT ntSym) (StripEOF (Sym $(conT nameToken))) $(conT nameToken) $(conT nameAST)
      glrParse :: ($(conT tokName) -> Bool) -> [Char]
                  -> LR.GLRResult
                      --(LR.CoreLR1State $(conT ntSym) (StripEOF (Sym $(conT nameToken))))
                      Int
                      Char
                      $(conT nameToken)
                      $(conT nameAST)
      glrParse filterF = (LR.glrParseInc2 $(varE nameUnit) event2ast (T.tokenizeInc filterF $(varE nameDFAs) lexeme2value))

      {- instance ALL.Token $(conT nameToken) where
        type Label $(conT nameToken) = StripEOF (Sym $(conT nameToken))
        getLabel = fromJust . stripEOF . getSymbol

        type Literal $(conT nameToken) = $(conT tokVal)
        getLiteral = T.tokenValue -}

      allstarParse :: [$(conT nameToken)] -> Either String $(conT nameAST)
      allstarParse inp = ALL.parse inp (ALL.NT $(s0 ast)) (ALL.atnOf ($(varE nameUnit) :: $(justGrammarTy ast unitTy))) True

      |]
  return $ decls ++ ast2DTFncns

-- | Support for this is __very__ experimental. This function allows you
--   to splice in compile-time computed versions of the LR1 data structures
--   so as to decrease the runtime of at-runtime parsing.
--   See @test/g4/G4.hs@ and @test/g4/Main.hs@ in the antlr-haskell source for
--   example usage of the @glrParseFast@ function generated.
mkLRParser ast g =
  let
    nameDFAs  = mkName (mkLower $ gName ast ++ "DFAs")
    tokName   = mkName "TokenName"
    nameAST   = mkName (mkUpper $ grammarName ast ++ "AST")
    name = mkName $ mkLower (grammarName ast ++ "Grammar")
    is = sort $ S.toList $ LR.lr1Items g
    tbl       = LR.lr1Table g

    tblInt = LR.convTableInt tbl is
    (_lr1Table', errs) = LR.disambiguate tblInt
    lr1Table' = M.toList tblInt -- _lr1Table'
    lr1S0'    = LR.convStateInt is $ LR.lr1Closure g $ LR.lr1S0 g

    unitTy = [t| () |]
    name' = [e| $(varE name) |] -- :: $(justGrammarTy' ast unitTy) |]
  in do --D.traceM $ pshow' is
        D.traceM $ "lr1S0 = " ++ (pshow' $ LR.lr1S0 g)
        --D.traceM $ "lr1Table = " ++ (pshow' $ LR.lr1Table g)
        D.traceM $ "lr1S0' = " ++ (pshow' lr1S0')
        D.traceM $ "lr1Table' = " ++ (pshow' lr1Table')
        D.traceM $ "Total LR1 conflicts: " ++ (pshow' errs)
          --
          --glrParse filterF = (LR.glrParseInc2 $(varE nameUnit) event2ast (T.tokenizeInc filterF $(varE nameDFAs) lexeme2value))
        --D.traceM $ "disambiguate tbl = " ++ (pshow' $ disambiguate tbl)
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
                (M.fromList' lr1Table)
                lr1Goto
                lr1S0
                (LR.tokenizerFirstSets convState $(name'))
                event2ast
                (T.tokenizeInc filterF $(varE nameDFAs) lexeme2value)
            |]

