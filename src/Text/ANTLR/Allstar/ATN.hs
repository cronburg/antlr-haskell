module Text.ANTLR.Allstar.ATN where
-- Augmented Transition Network
import Text.ANTLR.Allstar.Grammar
--import Text.ANTLR.Allstar.GSS hiding (Edge, Node)
import Text.ANTLR.Allstar.Stacks
import Data.Set (Set(..), empty, fromList, toList)

type Gamma = Stacks ATNState

data ATN s = ATN
  -- q  :: Set ATNState
  -- Σ is an alphabet consisting of distinct elements which are comparable for
  -- equality.
  -- _Σ :: Set (Edge s)
  { _Δ :: Set (Transition s)
  } deriving (Eq, Ord)

instance Show (ATN s) where
  show ATN {_Δ = _Δ} = "\n[" ++ (concatMap (\s -> "\n, " ++ show s) (toList _Δ)) ++ "\n]"

-- Tuple corresponding to a distinct transition in the ATN:
type Transition s = (ATNState, Edge s, ATNState)

-- The possible subscripts from Figure 8 of the ALL(*) paper
data ATNState = Start  NonTerminal
              | Middle NonTerminal Int Int
              | Accept NonTerminal
  deriving (Eq, Ord, Show)

sigma :: ATN s -> [Edge s]
sigma = undefined

e :: ATN s -> Set ATNState
e = undefined

f :: ATN s -> Set ATNState
f = undefined

data Edge s = NTE NonTerminal
            | TE  Terminal
            | PE  (Predicate s)
            | ME  (Mutator   s)
            | Epsilon
  deriving (Eq, Ord, Show)

-- atnOf :: Grammar -> (ATNState,Edge) -> Maybe ATNState
atnOf :: Grammar s -> ATN s
atnOf g = let

  _Δ :: Int -> Production s -> [Transition s]
  _Δ i (lhs, rhs) = let
  --(Prod _α)) = let

    -- Construct an internal production state from the given ATN identifier
    st :: NonTerminal -> Int -> Int -> ATNState
    st = Middle

    -- Create the transition for the k^th production element in the i^th
    -- production:
    _Δ' :: Int -> ProdElem -> Transition s
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
