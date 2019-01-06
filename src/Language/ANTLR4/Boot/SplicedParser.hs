{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, QuasiQuotes
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances #-}
{-|
  Module      : Language.ANTLR4.Boot.SplicedParser
  Description : Module as compiled by the core G4 quasiquoter
  Copyright   : (c) Karl Cronburg, 2018
  License     : BSD3
  Maintainer  : karl@cs.tufts.edu
  Stability   : experimental
  Portability : POSIX
-}
module Language.ANTLR4.Boot.SplicedParser where
import Text.ANTLR.Grammar
import Text.ANTLR.Parser
import qualified Text.ANTLR.LR as LR
--import Language.Chisel.Tokenizer
import Text.ANTLR.Lex.Tokenizer as T
import qualified Text.ANTLR.Set as S
import Text.ANTLR.Set (Hashable(..), Generic(..))
import Text.ANTLR.Pretty
import Control.Arrow ( (&&&) )
import Text.ANTLR.Lex.Regex (regex2dfa, Regex(..))
import Text.ANTLR.Lex (Token(..))
import Text.ANTLR.Allstar (parse, atnOf)
import Data.Maybe (fromJust)
import qualified Text.ANTLR.LL1
import qualified Text.ANTLR.Allstar as ALL

import Language.Haskell.TH.Quote (QuasiQuoter(..))
import qualified Language.Haskell.TH as TH
import Language.ANTLR4.Syntax
import qualified Language.ANTLR4.Boot.Syntax  as G4S

import Debug.Trace as D

-- | Construct a list from a single element
list a = [a]
cons = (:)
lexemeDirective r d = G4S.LRHS r (Just d)
lexemeNoDir     r   = G4S.LRHS r Nothing
lexDecl = G4S.Lex Nothing
lexFragment = G4S.Lex (Just G4S.Fragment)

literalRegex :: String -> G4S.Regex Char
literalRegex = G4S.Literal

prodDirective as d = G4S.PRHS as Nothing Nothing (Just d)
prodNoDir     as   = G4S.PRHS as Nothing Nothing Nothing

list2 a b = [a,b]
range a b = [a .. b]

gterm         = G4S.GTerm    G4S.NoAnnot
gnonTerm      = G4S.GNonTerm G4S.NoAnnot

maybeGTerm    = G4S.GTerm    (G4S.Regular '?')
maybeGNonTerm = G4S.GNonTerm (G4S.Regular '?')

starGTerm    = G4S.GTerm    (G4S.Regular '*')
starGNonTerm = G4S.GNonTerm (G4S.Regular '*')

plusGTerm    = G4S.GTerm    (G4S.Regular '+')
plusGNonTerm = G4S.GNonTerm (G4S.Regular '+')

regexAnyChar = G4S.Negation (G4S.CharSet [])

data G4NTSymbol
  = NT_decls |
    NT_decl1 |
    NT_prods |
    NT_lexemeRHS |
    NT_prodRHS |
    NT_regexes1 |
    NT_directive |
    NT_alphas |
    NT_alpha |
    NT_regexes |
    NT_regex |
    NT_regex1 |
    NT_charSet |
    NT_unionR |
    NT_charSet1
  deriving (Eq, Ord, Show, Hashable, Generic, Bounded, Enum)
data G4TSymbol
  = T_0 |
    T_1 |
    T_2 |
    T_3 |
    T_4 |
    T_5 |
    T_6 |
    T_7 |
    T_8 |
    T_9 |
    T_10 |
    T_11 |
    T_12 |
    T_13 |
    T_14 |
    T_15 |
    T_UpperID |
    T_LowerID |
    T_Literal |
    T_LineComment |
    T_EscapedChar |
    T_SetChar |
    T_WS
  deriving (Eq, Ord, Show, Hashable, Generic, Bounded, Enum)
g4Grammar' ::
  Prettify s_aLE0 => Grammar s_aLE0 G4NTSymbol G4TSymbol String
g4Grammar'
  = (defaultGrammar NT_decls :: Grammar s_aLE0 G4NTSymbol G4TSymbol String)
    {ns = S.fromList [minBound .. maxBound :: G4NTSymbol],
     ts = S.fromList [minBound .. maxBound :: G4TSymbol],
     ps = [(Production NT_decls) ((Prod Pass) [NT NT_decl1, T T_0]) (Just ""),
           (Production NT_decls)
             ((Prod Pass) [NT NT_decl1, T T_0, NT NT_decls]) (Just ""),
           (Production NT_decl1) ((Prod Pass) [T T_1, T T_UpperID]) (Just ""),
           (Production NT_decl1)
             ((Prod Pass) [T T_LowerID, T T_2, NT NT_prods]) (Just ""),
           (Production NT_decl1)
             ((Prod Pass) [T T_UpperID, T T_2, NT NT_lexemeRHS]) (Just ""),
           (Production NT_decl1)
             ((Prod Pass) [T T_3, T T_UpperID, T T_2, NT NT_lexemeRHS]) (Just ""),
           (Production NT_prods) ((Prod Pass) [NT NT_prodRHS]) (Just ""),
           (Production NT_prods)
             ((Prod Pass) [NT NT_prodRHS, T T_4, NT NT_prods]) (Just ""),
           (Production NT_lexemeRHS)
             ((Prod Pass) [NT NT_regexes1, T T_5, NT NT_directive]) (Just ""),
           (Production NT_lexemeRHS) ((Prod Pass) [NT NT_regexes1]) (Just ""),
           (Production NT_prodRHS)
             ((Prod Pass) [NT NT_alphas, T T_5, NT NT_directive]) (Just ""),
           (Production NT_prodRHS) ((Prod Pass) [NT NT_alphas]) (Just ""),
           (Production NT_directive) ((Prod Pass) [T T_UpperID]) (Just ""),
           (Production NT_directive) ((Prod Pass) [T T_LowerID]) (Just ""),
           (Production NT_directive) ((Prod Pass) [T T_UpperID, T T_14, NT NT_directive]) (Just ""),
           (Production NT_alphas) ((Prod Pass) [NT NT_alpha]) (Just ""),
           (Production NT_alphas) ((Prod Pass) [NT NT_alpha, NT NT_alphas]) (Just ""),
           (Production NT_alpha) ((Prod Pass) [T T_Literal, T T_6]) (Just ""),
           (Production NT_alpha) ((Prod Pass) [T T_LowerID, T T_6]) (Just ""),
           (Production NT_alpha) ((Prod Pass) [T T_UpperID, T T_6]) (Just ""),
           (Production NT_alpha) ((Prod Pass) [T T_Literal, T T_7]) (Just ""),
           (Production NT_alpha) ((Prod Pass) [T T_LowerID, T T_7]) (Just ""),
           (Production NT_alpha) ((Prod Pass) [T T_UpperID, T T_7]) (Just ""),
           (Production NT_alpha) ((Prod Pass) [T T_Literal, T T_8]) (Just ""),
           (Production NT_alpha) ((Prod Pass) [T T_LowerID, T T_8]) (Just ""),
           (Production NT_alpha) ((Prod Pass) [T T_UpperID, T T_8]) (Just ""),
           (Production NT_alpha) ((Prod Pass) [T T_Literal]) (Just ""),
           (Production NT_alpha) ((Prod Pass) [T T_LowerID]) (Just ""),
           (Production NT_alpha) ((Prod Pass) [T T_UpperID]) (Just ""),
           (Production NT_regexes1) ((Prod Pass) [NT NT_regexes]) (Just ""),
           (Production NT_regexes) ((Prod Pass) [NT NT_regex]) (Just ""),
           (Production NT_regexes) ((Prod Pass) [NT NT_regex, NT NT_regexes]) (Just ""),
           (Production NT_regex) ((Prod Pass) [NT NT_regex1, T T_6]) (Just ""),
           (Production NT_regex) ((Prod Pass) [NT NT_regex1, T T_7]) (Just ""),
           (Production NT_regex) ((Prod Pass) [NT NT_regex1, T T_8]) (Just ""),
           (Production NT_regex) ((Prod Pass) [T T_9, NT NT_regex1]) (Just ""),
           (Production NT_regex) ((Prod Pass) [NT NT_regex1]) (Just ""),
           (Production NT_regex1)
             ((Prod Pass) [T T_10, NT NT_charSet, T T_11]) (Just ""),
           (Production NT_regex1) ((Prod Pass) [T T_Literal]) (Just ""),
           (Production NT_regex1) ((Prod Pass) [T T_UpperID]) (Just ""),
           (Production NT_regex1)
             ((Prod Pass) [T T_12, NT NT_regexes1, T T_13]) (Just ""),
           (Production NT_regex1) ((Prod Pass) [NT NT_unionR]) (Just ""),
           (Production NT_regex1) ((Prod Pass) [T T_14]) (Just ""),
           (Production NT_unionR)
             ((Prod Pass) [NT NT_regex, T T_4, NT NT_regex]) (Just ""),
           (Production NT_unionR)
             ((Prod Pass) [NT NT_regex, T T_4, NT NT_unionR]) (Just ""),
           (Production NT_charSet) ((Prod Pass) [NT NT_charSet1]) (Just ""),
           (Production NT_charSet)
             ((Prod Pass) [NT NT_charSet1, NT NT_charSet]) (Just ""),
           (Production NT_charSet1)
             ((Prod Pass) [T T_SetChar, T T_15, T T_SetChar]) (Just ""),
           (Production NT_charSet1) ((Prod Pass) [T T_SetChar]) (Just ""),
           (Production NT_charSet1) ((Prod Pass) [T T_EscapedChar]) (Just "")]}
g4Grammar :: Grammar () G4NTSymbol G4TSymbol String
g4Grammar = Text.ANTLR.LL1.removeEpsilons g4Grammar'
type TokenName = G4TSymbol
data TokenValue
  = V_UpperID String |
    V_LowerID String |
    V_Literal String |
    V_LineComment String |
    V_EscapedChar Char |
    V_SetChar Char |
    V_WS String |
    V_0 |
    V_1 |
    V_2 |
    V_3 |
    V_4 |
    V_5 |
    V_6 |
    V_7 |
    V_8 |
    V_9 |
    V_10 |
    V_11 |
    V_12 |
    V_13 |
    V_14 |
    V_15
  deriving (Show, Ord, Eq, Generic, Hashable)
prettifyT_aLDY T_0 = pStr "';'"
prettifyT_aLDY T_1 = pStr "'grammar'"
prettifyT_aLDY T_2 = pStr "':'"
prettifyT_aLDY T_3 = pStr "'fragment'"
prettifyT_aLDY T_4 = pStr "'|'"
prettifyT_aLDY T_5 = pStr "'->'"
prettifyT_aLDY T_6 = pStr "'?'"
prettifyT_aLDY T_7 = pStr "'*'"
prettifyT_aLDY T_8 = pStr "'+'"
prettifyT_aLDY T_9 = pStr "'~'"
prettifyT_aLDY T_10 = pStr "'['"
prettifyT_aLDY T_11 = pStr "']'"
prettifyT_aLDY T_12 = pStr "'('"
prettifyT_aLDY T_13 = pStr "')'"
prettifyT_aLDY T_14 = pStr "'.'"
prettifyT_aLDY T_15 = pStr "'-'"
prettifyT_aLDY T_UpperID = pStr "UpperID"
prettifyT_aLDY T_LowerID = pStr "LowerID"
prettifyT_aLDY T_Literal = pStr "Literal"
prettifyT_aLDY T_LineComment = pStr "LineComment"
prettifyT_aLDY T_EscapedChar = pStr "EscapedChar"
prettifyT_aLDY T_SetChar = pStr "SetChar"
prettifyT_aLDY T_WS = pStr "WS"
prettifyValue_aLDZ V_0 = pStr "';'"
prettifyValue_aLDZ V_1 = pStr "'grammar'"
prettifyValue_aLDZ V_2 = pStr "':'"
prettifyValue_aLDZ V_3 = pStr "'fragment'"
prettifyValue_aLDZ V_4 = pStr "'|'"
prettifyValue_aLDZ V_5 = pStr "'->'"
prettifyValue_aLDZ V_6 = pStr "'?'"
prettifyValue_aLDZ V_7 = pStr "'*'"
prettifyValue_aLDZ V_8 = pStr "'+'"
prettifyValue_aLDZ V_9 = pStr "'~'"
prettifyValue_aLDZ V_10 = pStr "'['"
prettifyValue_aLDZ V_11 = pStr "']'"
prettifyValue_aLDZ V_12 = pStr "'('"
prettifyValue_aLDZ V_13 = pStr "')'"
prettifyValue_aLDZ V_14 = pStr "'.'"
prettifyValue_aLDZ V_15 = pStr "'-'"
prettifyValue_aLDZ (V_UpperID v)
  = (((pChr '\'') >> (prettify v)) >> (pChr '\''))
prettifyValue_aLDZ (V_LowerID v)
  = (((pChr '\'') >> (prettify v)) >> (pChr '\''))
prettifyValue_aLDZ (V_Literal v)
  = (((pChr '\'') >> (prettify v)) >> (pChr '\''))
prettifyValue_aLDZ (V_LineComment v)
  = (((pChr '\'') >> (prettify v)) >> (pChr '\''))
prettifyValue_aLDZ (V_EscapedChar v)
  = (((pChr '\'') >> (prettify v)) >> (pChr '\''))
prettifyValue_aLDZ (V_SetChar v)
  = (((pChr '\'') >> (prettify v)) >> (pChr '\''))
prettifyValue_aLDZ (V_WS v)
  = (((pChr '\'') >> (prettify v)) >> (pChr '\''))
instance Prettify G4NTSymbol where
  prettify = rshow
instance Prettify G4TSymbol where
  prettify = prettifyT_aLDY
instance Prettify TokenValue where
  prettify = prettifyValue_aLDZ
lookupToken ";" = ((Token T_0) V_0) 1
lookupToken "grammar" = ((Token T_1) V_1) 7
lookupToken ":" = ((Token T_2) V_2) 1
lookupToken "fragment" = ((Token T_3) V_3) 8
lookupToken "|" = ((Token T_4) V_4) 1
lookupToken "->" = ((Token T_5) V_5) 2
lookupToken "?" = ((Token T_6) V_6) 1
lookupToken "*" = ((Token T_7) V_7) 1
lookupToken "+" = ((Token T_8) V_8) 1
lookupToken "~" = ((Token T_9) V_9) 1
lookupToken "[" = ((Token T_10) V_10) 1
lookupToken "]" = ((Token T_11) V_11) 1
lookupToken "(" = ((Token T_12) V_12) 1
lookupToken ")" = ((Token T_13) V_13) 1
lookupToken "." = ((Token T_14) V_14) 1
lookupToken "-" = ((Token T_15) V_15) 1
lookupToken s = error ("Error: '" ++ (s ++ "' is not a token"))
lexeme2value l T_0 = V_0
lexeme2value l T_1 = V_1
lexeme2value l T_2 = V_2
lexeme2value l T_3 = V_3
lexeme2value l T_4 = V_4
lexeme2value l T_5 = V_5
lexeme2value l T_6 = V_6
lexeme2value l T_7 = V_7
lexeme2value l T_8 = V_8
lexeme2value l T_9 = V_9
lexeme2value l T_10 = V_10
lexeme2value l T_11 = V_11
lexeme2value l T_12 = V_12
lexeme2value l T_13 = V_13
lexeme2value l T_14 = V_14
lexeme2value l T_15 = V_15
lexeme2value l T_UpperID = V_UpperID l
lexeme2value l T_LowerID = V_LowerID l
lexeme2value l T_Literal
  = V_Literal
      ((stripQuotesReadEscape l :: String))
lexeme2value l T_LineComment = V_LineComment l
lexeme2value l T_EscapedChar
  = V_EscapedChar
      ((readEscape l :: Char))
lexeme2value l T_SetChar
  = V_SetChar ((head l :: Char))
lexeme2value l T_WS = V_WS l
g4Regexes
  = [(T_0, Text.ANTLR.Lex.Regex.Symbol ';'),
     (T_1, Text.ANTLR.Lex.Regex.Literal "grammar"),
     (T_2, Text.ANTLR.Lex.Regex.Symbol ':'),
     (T_3, Text.ANTLR.Lex.Regex.Literal "fragment"),
     (T_4, Text.ANTLR.Lex.Regex.Symbol '|'),
     (T_5, Text.ANTLR.Lex.Regex.Literal "->"),
     (T_6, Text.ANTLR.Lex.Regex.Symbol '?'),
     (T_7, Text.ANTLR.Lex.Regex.Symbol '*'),
     (T_8, Text.ANTLR.Lex.Regex.Symbol '+'),
     (T_9, Text.ANTLR.Lex.Regex.Symbol '~'),
     (T_10, Text.ANTLR.Lex.Regex.Symbol '['),
     (T_11, Text.ANTLR.Lex.Regex.Symbol ']'),
     (T_12, Text.ANTLR.Lex.Regex.Symbol '('),
     (T_13, Text.ANTLR.Lex.Regex.Symbol ')'),
     (T_14, Text.ANTLR.Lex.Regex.Symbol '.'),
     (T_15, Text.ANTLR.Lex.Regex.Symbol '-'),
     (T_UpperID,
      Text.ANTLR.Lex.Regex.Concat
        [Text.ANTLR.Lex.Regex.Class "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
         Text.ANTLR.Lex.Regex.Kleene
           (Text.ANTLR.Lex.Regex.Class
              "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_")]),
     (T_LowerID,
      Text.ANTLR.Lex.Regex.Concat
        [Text.ANTLR.Lex.Regex.Class "abcdefghijklmnopqrstuvwxyz",
         Text.ANTLR.Lex.Regex.Kleene
           (Text.ANTLR.Lex.Regex.Class
              "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_")]),
     (T_Literal,
      Text.ANTLR.Lex.Regex.Concat
        [Text.ANTLR.Lex.Regex.Symbol '\'',
         Text.ANTLR.Lex.Regex.Concat
           [Text.ANTLR.Lex.Regex.PosClos
              (Union (Literal "\\'")
                     (NotClass "'")), --Text.ANTLR.Lex.Regex.NotClass "'"),
            Text.ANTLR.Lex.Regex.Symbol '\'']]),
     (T_LineComment,
      Text.ANTLR.Lex.Regex.Concat
        [Text.ANTLR.Lex.Regex.Literal "//",
         Text.ANTLR.Lex.Regex.Concat
           [Text.ANTLR.Lex.Regex.Kleene (Text.ANTLR.Lex.Regex.NotClass "\n"),
            Text.ANTLR.Lex.Regex.Symbol '\n']]),
     (T_EscapedChar,
      Text.ANTLR.Lex.Regex.Concat
        [Text.ANTLR.Lex.Regex.Symbol '\\',
         Text.ANTLR.Lex.Regex.Class "tnrfv"]),
     (T_SetChar, Text.ANTLR.Lex.Regex.NotClass "]"),
     (T_WS,
      Text.ANTLR.Lex.Regex.PosClos
        (Text.ANTLR.Lex.Regex.Class " \t\n\r\f\v"))]
g4DFAs = (map (fst &&& (regex2dfa . snd))) g4Regexes
type G4AST = AST G4NTSymbol G4Token
type G4Token = Token G4TSymbol TokenValue
instance Ref G4NTSymbol where
  type Sym G4NTSymbol = G4NTSymbol
  getSymbol = id
tokenize_aLE7 :: String -> [G4Token]
tokenize_aLE7 = (tokenize g4DFAs) lexeme2value
slrParse_aLE6 ::
  [G4Token]
  -> LR.LRResult (LR.CoreSLRState G4NTSymbol (StripEOF (Sym G4Token))) G4Token G4AST
slrParse_aLE6 = (LR.slrParse g4Grammar) event2ast
glrParse_aLE5 ::
  (TokenName -> Bool)
  -> [Char]
     -> LR.LR1Result (LR.CoreLR1State G4NTSymbol (StripEOF (Sym G4Token))) Char G4AST
glrParse_aLE5 filterF_aLE8
  = ((LR.glrParseInc g4Grammar) event2ast)
      (((tokenizeInc filterF_aLE8) g4DFAs) lexeme2value)
instance ALL.Token G4Token where
  type Label G4Token = StripEOF (Sym G4Token)
  type Literal G4Token = TokenValue
  getLabel
    = (Data.Maybe.fromJust . (stripEOF . getSymbol))
  getLiteral = tokenValue
allstarParse_aLE4 :: [G4Token] -> Either String G4AST
allstarParse_aLE4 inp_aLE9
  = (((Text.ANTLR.Allstar.parse inp_aLE9)
        (ALL.NT NT_decls))
       (Text.ANTLR.Allstar.atnOf
          (g4Grammar :: Grammar () G4NTSymbol G4TSymbol String)))
      True
ast2EscapedChar (Leaf (Token _ (V_EscapedChar t) _)) = t
ast2LineComment (Leaf (Token _ (V_LineComment t) _)) = t
ast2Literal (Leaf (Token _ (V_Literal t) _)) = t
ast2LowerID (Leaf (Token _ (V_LowerID t) _)) = t
ast2SetChar (Leaf (Token _ (V_SetChar t) _)) = t
ast2UpperID (Leaf (Token _ (V_UpperID t) _)) = t
ast2WS (Leaf (Token _ (V_WS t) _)) = t
ast2alpha (AST NT_alpha [T T_Literal, T T_6] [v0_Literal, _])
  = maybeGTerm (ast2Literal v0_Literal)
ast2alpha (AST NT_alpha [T T_LowerID, T T_6] [v0_LowerID, _])
  = maybeGNonTerm (ast2LowerID v0_LowerID)
ast2alpha (AST NT_alpha [T T_UpperID, T T_6] [v0_UpperID, _])
  = maybeGNonTerm (ast2UpperID v0_UpperID)
ast2alpha (AST NT_alpha [T T_Literal, T T_7] [v0_Literal, _])
  = starGTerm (ast2Literal v0_Literal)
ast2alpha (AST NT_alpha [T T_LowerID, T T_7] [v0_LowerID, _])
  = starGNonTerm (ast2LowerID v0_LowerID)
ast2alpha (AST NT_alpha [T T_UpperID, T T_7] [v0_UpperID, _])
  = starGNonTerm (ast2UpperID v0_UpperID)
ast2alpha (AST NT_alpha [T T_Literal, T T_8] [v0_Literal, _])
  = plusGTerm (ast2Literal v0_Literal)
ast2alpha (AST NT_alpha [T T_LowerID, T T_8] [v0_LowerID, _])
  = plusGNonTerm (ast2LowerID v0_LowerID)
ast2alpha (AST NT_alpha [T T_UpperID, T T_8] [v0_UpperID, _])
  = plusGNonTerm (ast2UpperID v0_UpperID)
ast2alpha (AST NT_alpha [T T_Literal] [v0_Literal])
  = gterm (ast2Literal v0_Literal)
ast2alpha (AST NT_alpha [T T_LowerID] [v0_LowerID])
  = gnonTerm (ast2LowerID v0_LowerID)
ast2alpha (AST NT_alpha [T T_UpperID] [v0_UpperID])
  = gnonTerm (ast2UpperID v0_UpperID)
ast2alpha ast2 = error (show ast2)
ast2alphas (AST NT_alphas [NT NT_alpha] [v0_alpha])
  = list (ast2alpha v0_alpha)
ast2alphas
  (AST NT_alphas [NT NT_alpha, NT NT_alphas] [v0_alpha, v1_alphas])
  = (cons (ast2alpha v0_alpha)) (ast2alphas v1_alphas)
ast2alphas ast2 = error (show ast2)
ast2charSet (AST NT_charSet [NT NT_charSet1] [v0_charSet1])
  = id (ast2charSet1 v0_charSet1)
ast2charSet
  (AST NT_charSet
       [NT NT_charSet1, NT NT_charSet]
       [v0_charSet1, v1_charSet])
  = ((++) (ast2charSet1 v0_charSet1)) (ast2charSet v1_charSet)
ast2charSet ast2 = error (show ast2)
ast2charSet1
  (AST NT_charSet1
       [T T_SetChar, T T_15, T T_SetChar]
       [v0_SetChar, _, v2_SetChar])
  = (range (ast2SetChar v0_SetChar)) (ast2SetChar v2_SetChar)
ast2charSet1 (AST NT_charSet1 [T T_SetChar] [v0_SetChar])
  = list (ast2SetChar v0_SetChar)
ast2charSet1 (AST NT_charSet1 [T T_EscapedChar] [v0_EscapedChar])
  = list (ast2EscapedChar v0_EscapedChar)
ast2charSet1 ast2 = error (show ast2)
ast2decl1 (AST NT_decl1 [T T_1, T T_UpperID] [_, v1_UpperID])
  = G4S.Grammar (ast2UpperID v1_UpperID)
ast2decl1
  (AST NT_decl1
       [T T_LowerID, T T_2, NT NT_prods]
       [v0_LowerID, _, v2_prods])
  = (G4S.Prod (ast2LowerID v0_LowerID)) (ast2prods v2_prods)
ast2decl1
  (AST NT_decl1
       [T T_UpperID, T T_2, NT NT_lexemeRHS]
       [v0_UpperID, _, v2_lexemeRHS])
  = (lexDecl (ast2UpperID v0_UpperID)) (ast2lexemeRHS v2_lexemeRHS)
ast2decl1
  (AST NT_decl1
       [T T_3, T T_UpperID, T T_2, NT NT_lexemeRHS]
       [_, v1_UpperID, _, v3_lexemeRHS])
  = (lexFragment (ast2UpperID v1_UpperID))
      (ast2lexemeRHS v3_lexemeRHS)
ast2decl1 ast2 = error (show ast2)
ast2decls (AST NT_decls [NT NT_decl1, T T_0] [v0_decl1, _])
  = list (ast2decl1 v0_decl1)
ast2decls
  (AST NT_decls
       [NT NT_decl1, T T_0, NT NT_decls]
       [v0_decl1, _, v2_decls])
  = (cons (ast2decl1 v0_decl1)) (ast2decls v2_decls)
ast2decls ast2 = error (show ast2)
ast2directive (AST NT_directive [T T_UpperID] [v0_UpperID])
  = G4S.UpperD $ ast2UpperID v0_UpperID
ast2directive (AST NT_directive [T T_LowerID] [v0_LowerID])
  = G4S.LowerD $ ast2LowerID v0_LowerID
ast2directive (AST NT_directive [T T_UpperID, T T_14, NT NT_directive] [v0_UpperID, _, v1_dir])
  = G4S.UpperD $ (ast2UpperID v0_UpperID) ++ "." ++ ((\(G4S.UpperD s) -> s) (ast2directive v1_dir))
ast2directive ast2 = error (show ast2)
ast2lexemeRHS
  (AST NT_lexemeRHS
       [NT NT_regexes1, T T_5, NT NT_directive]
       [v0_regexes1, _, v2_directive])
  = (lexemeDirective (ast2regexes1 v0_regexes1))
      (ast2directive v2_directive)
ast2lexemeRHS (AST NT_lexemeRHS [NT NT_regexes1] [v0_regexes1])
  = lexemeNoDir (ast2regexes1 v0_regexes1)
ast2lexemeRHS ast2 = error (show ast2)
ast2prodRHS
  (AST NT_prodRHS
       [NT NT_alphas, T T_5, NT NT_directive]
       [v0_alphas, _, v2_directive])
  = (prodDirective (ast2alphas v0_alphas))
      (ast2directive v2_directive)
ast2prodRHS (AST NT_prodRHS [NT NT_alphas] [v0_alphas])
  = prodNoDir (ast2alphas v0_alphas)
ast2prodRHS ast2 = error (show ast2)
ast2prods (AST NT_prods [NT NT_prodRHS] [v0_prodRHS])
  = list (ast2prodRHS v0_prodRHS)
ast2prods
  (AST NT_prods
       [NT NT_prodRHS, T T_4, NT NT_prods]
       [v0_prodRHS, _, v2_prods])
  = (cons (ast2prodRHS v0_prodRHS)) (ast2prods v2_prods)
ast2prods ast2 = error (show ast2)
ast2regex (AST NT_regex [NT NT_regex1, T T_6] [v0_regex1, _])
  = G4S.Question (ast2regex1 v0_regex1)
ast2regex (AST NT_regex [NT NT_regex1, T T_7] [v0_regex1, _])
  = G4S.Kleene (ast2regex1 v0_regex1)
ast2regex (AST NT_regex [NT NT_regex1, T T_8] [v0_regex1, _])
  = G4S.PosClos (ast2regex1 v0_regex1)
ast2regex (AST NT_regex [T T_9, NT NT_regex1] [_, v1_regex1])
  = G4S.Negation (ast2regex1 v1_regex1)
ast2regex (AST NT_regex [NT NT_regex1] [v0_regex1])
  = id (ast2regex1 v0_regex1)
ast2regex ast2 = error (show ast2)
ast2regex1
  (AST NT_regex1 [T T_10, NT NT_charSet, T T_11] [_, v1_charSet, _])
  = G4S.CharSet (ast2charSet v1_charSet)
ast2regex1 (AST NT_regex1 [T T_Literal] [v0_Literal])
  = literalRegex (ast2Literal v0_Literal)
ast2regex1 (AST NT_regex1 [T T_UpperID] [v0_UpperID])
  = G4S.Named (ast2UpperID v0_UpperID)
ast2regex1
  (AST NT_regex1
       [T T_12, NT NT_regexes1, T T_13]
       [_, v1_regexes1, _])
  = ast2regexes1 v1_regexes1
ast2regex1 (AST NT_regex1 [NT NT_unionR] [v0_unionR])
  = G4S.Union (ast2unionR v0_unionR)
ast2regex1 (AST NT_regex1 [T T_14] [_]) = regexAnyChar
ast2regex1 ast2 = error (show ast2)
ast2regexes (AST NT_regexes [NT NT_regex] [v0_regex])
  = list (ast2regex v0_regex)
ast2regexes
  (AST NT_regexes
       [NT NT_regex, NT NT_regexes]
       [v0_regex, v1_regexes])
  = (cons (ast2regex v0_regex)) (ast2regexes v1_regexes)
ast2regexes ast2 = error (show ast2)
ast2regexes1 (AST NT_regexes1 [NT NT_regexes] [v0_regexes])
  = G4S.Concat (ast2regexes v0_regexes)
ast2regexes1 ast2 = error (show ast2)
ast2unionR
  (AST NT_unionR
       [NT NT_regex, T T_4, NT NT_regex]
       [v0_regex, _, v2_regex])
  = (list2 (ast2regex v0_regex)) (ast2regex v2_regex)
ast2unionR
  (AST NT_unionR
       [NT NT_regex, T T_4, NT NT_unionR]
       [v0_regex, _, v2_unionR])
  = (cons (ast2regex v0_regex)) (ast2unionR v2_unionR)
ast2unionR ast2 = error (show ast2)

-----------------------------------------------------------------------------
isWhitespace T_LineComment = True
isWhitespace T_WS = True
isWhitespace _ = False

parseANTLR = glrParse_aLE5 isWhitespace

