{-# LANGUAGE ScopedTypeVariables, DeriveAnyClass, DeriveGeneric
           , FlexibleContexts, UndecidableInstances, StandaloneDeriving
           , OverloadedStrings #-}
{-|
  Module      : Text.ANTLR.Allstar.ATN
  Description : Augmented recursive transition network algorithms
  Copyright   : (c) Karl Cronburg, 2018
  License     : BSD3
  Maintainer  : karl@cs.tufts.edu
  Stability   : experimental
  Portability : POSIX

-}
module Text.ANTLR.Allstar.ATN where
-- Augmented recursive Transition Network
import Text.ANTLR.Grammar
--import Text.ANTLR.Allstar.GSS hiding (Edge, Node)
import Text.ANTLR.Allstar.Stacks
import Text.ANTLR.Set (Set(..), empty, fromList, toList, Hashable, Generic)
import Text.ANTLR.Pretty

-- | Graph-structured stack over ATN states.
type Gamma nt = Stacks (ATNState nt)

-- | An ATN defining some language we wish to parse
data ATN s nt t = ATN
  { _Δ :: Set (Transition s nt t) -- ^ The transition function
  } deriving (Eq, Ord, Show)

instance (Prettify s, Prettify nt, Prettify t, Hashable nt, Hashable t, Eq nt, Eq t) => Prettify (ATN s nt t) where
  prettify atn = do
    pLine "_Δ:"
    incrIndent 4
    prettify $ _Δ atn
    incrIndent (-4)

-- | Tuple corresponding to a distinct transition in the ATN:
type Transition s nt t = (ATNState nt, Edge s nt t, ATNState nt)

-- | The possible subscripts from Figure 8 of the ALL(*) paper
data ATNState nt  = Start  nt
                  | Middle nt Int Int
                  | Accept nt
  deriving (Eq, Generic, Hashable, Ord, Show)

-- | LaTeX style ATN states. TODO: check length of NT printed and put curly braces
-- around it if more than one character.
instance (Prettify nt) => Prettify (ATNState nt) where
  prettify (Start nt)  = pStr "p_"  >> prettify nt
  prettify (Accept nt) = pStr "p'_" >> prettify nt
  prettify (Middle nt i j) = do
    pStr "p_{"
    prettify i
    pStr ","
    prettify j
    pStr "}"

-- | An edge in an ATN.
data Edge s nt t =
    NTE nt              -- ^ Nonterminal edge
  | TE  t               -- ^ Terminal edge
  | PE  (Predicate ())  -- ^ Predicated edge with no state
  | ME  (Mutator   ())  -- ^ Mutator edge with no state
  | Epsilon             -- ^ Nondeterministic edge parsing nothing
  deriving (Eq, Generic, Hashable, Ord, Show)

instance (Prettify s, Prettify nt, Prettify t) => Prettify (Edge s nt t) where
  prettify x = do
    pStr "--"
    case x of
      NTE nt -> prettify nt
      TE   t -> prettify t
      PE   p -> prettify p
      ME   m -> prettify m
      Epsilon -> pStr "ε"
    pStr "-->"

-- | Convert a G4 grammar into an ATN for parsing with ALL(*)
atnOf
  :: forall nt t s dt. (Eq nt, Eq t, Hashable nt, Hashable t)
  => Grammar s nt t dt -> ATN s nt t
atnOf g = let

  _Δ :: Int -> Production s nt t dt -> [Transition s nt t]
  _Δ i (Production lhs rhs _) = let
  --(Prod _α)) = let

    -- Construct an internal production state from the given ATN identifier
    st :: nt -> Int -> Int -> ATNState nt
    st = Middle

    -- Create the transition for the k^th production element in the i^th
    -- production:
    _Δ' :: Int -> ProdElem nt t -> Transition s nt t
    _Δ' k (NT nt) = (st lhs i (k - 1), NTE nt, st lhs i k)
    _Δ' k (T  t)  = (st lhs i (k - 1), TE  t,  st lhs i k)

    -- The epsilon (or mu) transition for the accepting / final state:
    sϵ    = (Start lhs, Epsilon, Middle lhs i 0)
    fϵ _α = (Middle lhs i (length _α), Epsilon, Accept lhs)

    sem_state _α = Middle lhs i (length _α + 1)
    sϵ_sem _π _α = [(Start lhs, Epsilon, sem_state _α), (sem_state _α, PE _π, Middle lhs i 0)]
    fϵ_sem       = fϵ

    sϵ_mut    = sϵ
    fϵ_mut _μ = (Middle lhs i 0, ME _μ, Accept lhs)

    in  (case rhs of
          (Prod Pass _α)        -> [sϵ, fϵ _α]                 ++ zipWith _Δ' [1..(length _α)] _α
          (Prod (Sem _π) _α)    -> sϵ_sem _π _α ++ [fϵ_sem _α] ++ zipWith _Δ' [1..(length _α)] _α
          (Prod (Action _μ) _)  -> [sϵ_mut, fϵ_mut _μ]
        )

  in ATN
    { _Δ = fromList $ concat $ zipWith _Δ [0..length (ps g)] $ ps g
    }
