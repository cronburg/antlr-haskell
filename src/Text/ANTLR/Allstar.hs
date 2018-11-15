{-# LANGUAGE DeriveAnyClass, DeriveGeneric, FlexibleContexts #-}
module Text.ANTLR.Allstar
  ( parse
  , ALL.Token(..)
  , ALL.GrammarSymbol(..)
  , ALL.ATNEnv, atnOf
  ) where

import qualified Text.ANTLR.Allstar.ParserGenerator as ALL

import qualified Text.ANTLR.Parser as P
import qualified Text.ANTLR.Grammar as G
import qualified Text.ANTLR.Allstar.ATN as ATN

import qualified Data.Set as DS
import qualified Text.ANTLR.Set as S

-- | Go from an Allstar AST to the AST type used internally in this package
fromAllstarAST :: ALL.AST nts t -> P.AST nts t
fromAllstarAST (ALL.Node nt asts) = P.AST nt [] (map fromAllstarAST asts)
fromAllstarAST (ALL.Leaf tok)     = P.Leaf tok

-- | Go from an antlr-haskell Grammar to an Allstar ATNEnv
--   TODO: Handle predicate and mutator state during the conversion
atnOf :: (Ord nt, Ord t, S.Hashable nt, S.Hashable t) => G.Grammar s nt t -> ALL.ATNEnv nt t
atnOf g = DS.fromList (map convTrans (S.toList (ATN._Δ (ATN.atnOf g))))

-- | ATN Transition to AllStar version
convTrans (st0, e, st1) = (convState st0, convEdge e, convState st1)

-- | ATN State to AllStar version
convState (ATN.Start nt)        = ALL.Init nt
convState (ATN.Middle nt i0 i1) = ALL.Middle nt i0 i1
convState (ATN.Accept nt)       = ALL.Final nt

-- | ATN Edge to AllStar version
convEdge (ATN.NTE nt) = ALL.GS (ALL.NT nt)
convEdge (ATN.TE t)   = ALL.GS (ALL.T t)
convEdge (ATN.PE p)   = ALL.PRED True -- TODO
convEdge (ATN.ME m)   = ALL.PRED True -- TODO
convEdge ATN.Epsilon  = ALL.GS ALL.EPS

parse inp s0 atns cache = fromAllstarAST <$> ALL.parse inp s0 atns cache

convSymbol s = ALL.NT s

toAllstarSymbol :: G.ProdElem nts ts -> ALL.GrammarSymbol nts ts
toAllstarSymbol (G.NT nts) = ALL.NT nts
toAllstarSymbol (G.T  ts)  = ALL.T  ts
toAllstarSymbol (G.Eps)    = ALL.EPS

