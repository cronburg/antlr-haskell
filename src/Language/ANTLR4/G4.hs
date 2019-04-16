{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, QuasiQuotes
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, DeriveDataTypeable
    , TemplateHaskell #-}
{-|
  Module      : Language.ANTLR4.G4
  Description : Core G4 quasiquoter for antlr-haskell
  Copyright   : (c) Karl Cronburg, 2018
  License     : BSD3
  Maintainer  : karl@cs.tufts.edu
  Stability   : experimental
  Portability : POSIX

  Until better haddock integration is developed, you'll need to look
  at the source for this module to see the G4 grammar for G4.
-}
module Language.ANTLR4.G4 where

import Control.Arrow ( (&&&) )
import Data.Char (isUpper)

import Text.ANTLR.Common
import Text.ANTLR.Grammar
import Text.ANTLR.Parser
import qualified Text.ANTLR.LR as LR
import Text.ANTLR.Lex.Tokenizer as T
import qualified Text.ANTLR.Set as S
import Text.ANTLR.Set (Hashable(..), Generic(..))
import Text.ANTLR.Pretty
import Text.ANTLR.Lex.Regex (regex2dfa)
import Data.Data (Data(..))
import Language.Haskell.TH.Lift (Lift(..))

import Language.Haskell.TH.Quote (QuasiQuoter(..))
import qualified Language.Haskell.TH as TH
import Language.ANTLR4.Boot.Quote (antlr4)
import Language.ANTLR4.Syntax
import qualified Language.ANTLR4.Boot.Syntax  as G4S
import qualified Language.ANTLR4.Boot.Quote   as G4Q

import Debug.Trace as D

char :: String -> Char
char = head

append :: String -> String -> String
append = (++)

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
prodNoAlphas     d = G4S.PRHS [] Nothing Nothing (Just d)
prodNothing        = G4S.PRHS [] Nothing Nothing Nothing

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

dQual [] = G4S.UpperD []
dQual xs = case last xs of
  [] -> G4S.UpperD $ concatWith "." xs
  (a:as)
    | isUpper a -> G4S.UpperD $ concatWith "." xs
    | otherwise -> G4S.LowerD $ concatWith "." xs

qDir l u = [l,u]

haskellD = G4S.HaskellD

-- Force the above declarations (and their types) into scope:
$( return [] )

[antlr4|
  grammar G4;

  decls : decl1 ';'                 -> list
        | decl1 ';' decls           -> cons
        ;

  decl1 : 'grammar' UpperID                 -> G4S.Grammar
        | LowerID ':' prods                 -> G4S.Prod
        | UpperID ':' lexemeRHS             -> lexDecl
        | 'fragment' UpperID ':' lexemeRHS  -> lexFragment
        ;

  prods : prodRHS                   -> list
        | prodRHS '|' prods         -> cons
        ;

  lexemeRHS : regexes1 '->' directive  -> lexemeDirective
            | regexes1                 -> lexemeNoDir
            ;

  prodRHS : alphas '->' directive   -> prodDirective
          | alphas                  -> prodNoDir
          |        '->' directive   -> prodNoAlphas
          |                         -> prodNothing
          ;

  directive : qDirective          -> dQual
            | UpperID             -> G4S.UpperD
            | LowerID             -> G4S.LowerD
            | '${' HaskellExp '}' -> haskellD
            ;

  qDirective  : UpperID '.' qDot -> qDir
              ;

  qDot  : UpperID
        | LowerID
        ;

  HaskellExp : ( ~ '}' )+ -> String;

  alphas : alpha                    -> list
         | alpha alphas             -> cons
         | '(' alphas ')'
         | '(' alphas ')' '?'
         | '(' alphas ')' '*'
         | '(' alphas ')' '+'
         ;

  alpha : Literal '?'               -> maybeGTerm
        | LowerID '?'               -> maybeGNonTerm
        | UpperID '?'               -> maybeGNonTerm
        | Literal '*'               -> starGTerm
        | LowerID '*'               -> starGNonTerm
        | UpperID '*'               -> starGNonTerm
        | Literal '+'               -> plusGTerm
        | LowerID '+'               -> plusGNonTerm
        | UpperID '+'               -> plusGNonTerm
        | Literal                   -> gterm
        | LowerID                   -> gnonTerm
        | UpperID                   -> gnonTerm
        ;

  // Regex Stuff:

  regexes1 : regexes                -> G4S.Concat
           ;

  regexes : regex                   -> list
          | regex regexes           -> cons
          ;

  regex   :     regex1 '?'          -> G4S.Question
          |     regex1 '*'          -> G4S.Kleene
          |     regex1 '+'          -> G4S.PosClos
          | '~' regex1              -> G4S.Negation
          |     regex1              -> id
          ;

  regex1  : '[' charSet ']'           -> G4S.CharSet
          | Literal                   -> literalRegex
          | UpperID                   -> G4S.Named
          | '(' regexes1 ')'
          | unionR                    -> G4S.Union
          | '.'                       -> regexAnyChar
          ;

  unionR  : regex '|' regex         -> list2
          | regex '|' unionR        -> cons
          ;

  charSet : charSet1                -> id
          | charSet1 charSet        -> append
          ;

  charSet1 : SetChar '-' SetChar    -> range
           | SetChar                -> list
           | EscapedChar            -> list
           ;

  UpperID : [A-Z][a-zA-Z0-9_]*      -> String;
  LowerID : [a-z][a-zA-Z0-9_]*      -> String;
  Literal     : '\'' (~ '\'')+ '\'' -> stripQuotesReadEscape;
  LineComment : '//' (~ '\n')* '\n' -> String;

  SetChar     : ~ ']'               -> char ;
  WS          : [ \t\n\r\f\v]+      -> String;
  EscapedChar : '\\' [tnrfv]        -> readEscape ;

|]

