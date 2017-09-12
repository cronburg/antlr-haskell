{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
module Text.ANTLR.Allstar
  ( parse
  , Token(..)
  , GrammarSymbol(..)
  , ATNEnv, atnOf
  ) where

import ParserGenerator.AllStar

import qualified Text.ANTLR.Parser as P
import qualified Text.ANTLR.Grammar as G
import qualified Text.ANTLR.Allstar.ATN as ATN

import qualified Data.Set as DS
import qualified Text.ANTLR.Set as S

-- | Go from an Allstar AST to the AST type used internally in this package
fromAllstarAST :: AST nts t -> P.AST nts t
fromAllstarAST (Node nt asts) = P.AST nt [] (map fromAllstarAST asts)
fromAllstarAST (Leaf tok)     = P.Leaf tok

-- | Go from an antlr-haskell Grammar to an Allstar ATNEnv
--   TODO: Handle predicate and mutator state during the conversion
atnOf :: (Ord nt, Ord t, S.Hashable nt, S.Hashable t) => G.Grammar s nt t -> ATNEnv nt t
atnOf g = DS.fromList (map convTrans (S.toList (ATN._Δ (ATN.atnOf g))))

-- | ATN Transition to AllStar version
convTrans (st0, e, st1) = (convState st0, convEdge e, convState st1)

-- | ATN State to AllStar version
convState (ATN.Start nt)        = Init nt
convState (ATN.Middle nt i0 i1) = Middle nt i0 i1
convState (ATN.Accept nt)       = Final nt

-- | ATN Edge to AllStar version
convEdge (ATN.NTE nt) = GS (NT nt)
convEdge (ATN.TE t)   = GS (T t)
convEdge (ATN.PE p)   = PRED True -- TODO
convEdge (ATN.ME m)   = PRED True -- TODO
convEdge ATN.Epsilon  = GS EPS

