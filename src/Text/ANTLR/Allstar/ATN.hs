{-# LANGUAGE ScopedTypeVariables #-}
module Text.ANTLR.Allstar.ATN where
-- Augmented Transition Network
import Text.ANTLR.Allstar.Grammar
--import Text.ANTLR.Allstar.GSS hiding (Edge, Node)
import Text.ANTLR.Allstar.Stacks
import Data.Set (Set(..), empty, fromList, toList)

type Gamma nt = Stacks (ATNState nt)

data ATN s nt t = ATN
  -- q  :: Set ATNState
  -- Σ is an alphabet consisting of distinct elements which are comparable for
  -- equality.
  -- _Σ :: Set (Edge s)
  { _Δ :: Set (Transition s nt t)
  } deriving (Eq, Ord)

instance (Show nt, Show t) => Show (ATN s nt t) where
  show ATN {_Δ = _Δ} = "\n[" ++ (concatMap (\s -> "\n, " ++ show s) (toList _Δ)) ++ "\n]"

-- Tuple corresponding to a distinct transition in the ATN:
type Transition s nt t = (ATNState nt, Edge s nt t, ATNState nt)

-- The possible subscripts from Figure 8 of the ALL(*) paper
data ATNState nt  = Start  nt
                  | Middle nt Int Int
                  | Accept nt
  deriving (Ord, Show)

{- ATNs do not leak the Terminal and NonTerminal abstractions -}
instance (NonTerminal nt) => Eq (ATNState nt) where
  Start nt == Start nt1             = sameNTs nt nt1
  Middle nt x y == Middle nt1 x1 y1 = sameNTs nt nt1 && x == x1 && y == y1
  Accept nt == Accept nt1           = sameNTs nt nt1
  x == y                            = False

sigma :: ATN s nt t -> [Edge s nt t]
sigma = undefined

e :: ATN s nt t -> Set (ATNState nt)
e = undefined

f :: ATN s nt t -> Set (ATNState nt)
f = undefined

data Edge s nt t = NTE nt
                 | TE  t
                 | PE  (Predicate s)
                 | ME  (Mutator   s)
                 | Epsilon
  deriving (Ord, Show)

{- ATNs do not leak the Terminal and NonTerminal abstractions -}
instance (NonTerminal nt, Terminal t) => Eq (Edge s nt t) where
  NTE nt == NTE nt1 = sameNTs nt nt1
  TE t == TE t1 = sameTokens t t1
  PE p == PE p1 = p == p1
  ME m == ME m1 = m == m1
  Epsilon == Epsilon = True
  x == y = False

-- atnOf :: Grammar -> (ATNState,Edge) -> Maybe ATNState
atnOf :: forall nt. forall t. forall s. (NonTerminal nt, Terminal t, Ord nt, Ord t) => Grammar s nt t -> ATN s nt t
atnOf g = let

  _Δ :: Int -> Production s nt t -> [Transition s nt t]
  _Δ i (lhs, rhs) = let
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
          (Prod _α)   -> [sϵ, fϵ _α]                 ++ zipWith _Δ' [1..(length _α)] _α
          (Sem _π _α) -> sϵ_sem _π _α ++ [fϵ_sem _α] ++ zipWith _Δ' [1..(length _α)] _α
          (Action _μ) -> [sϵ_mut, fϵ_mut _μ]
        )

  in ATN
    { _Δ = fromList $ concat $ zipWith _Δ [0..length (ps g)] $ ps g
    }
