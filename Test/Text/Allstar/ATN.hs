module Test.Text.Allstar.ATN where
import Text.Allstar.Grammar
import Text.Allstar.ATN
import Test.Text.Allstar.Grammar
import Data.Set (fromList)

-- This is the Grammar from page 6 of the
-- 'Adaptive LL(*) Parsing: The Power of Dynamic Analysis'
-- paper, namely the expected transitions based on Figures
-- 5 through 8:
paperATNGrammar = defaultGrammar
  { ns = fromList ["S", "A"]
  , ts = fromList ["a", "b", "c", "d"]
  , s0 = "C"
  , ps =
          [ Production "S" $ Prod [NT "A", T "c"]
          , Production "S" $ Prod [NT "A", T "d"]
          , Production "A" $ Prod [T "a", NT "A"]
          , Production "A" $ Prod [T "b"]
          ]
  }

s i = ("S", i)

-- Names as shown in paper:
pS  = Start  "S"
pS1 = Middle "S" 0 0
p1  = Middle "S" 0 1
p2  = Middle "S" 0 2
pS2 = Middle "S" 1 0
p3  = Middle "S" 1 1
p4  = Middle "S" 1 2
pS' = Accept "S"

pA  = Start  "A"
pA1 = Middle "A" 2 0
p5  = Middle "A" 2 1
p6  = Middle "A" 2 2
pA2 = Middle "A" 3 0
p7  = Middle "A" 3 1
pA' = Accept "A"

exp_paperATN = ATN
  { _Δ = fromList
    -- Submachine for S:
    [ (pS,  Epsilon, pS1)
    , (pS1, NTE "A", p1)
    , (p1,  TE  "c", p2)
    , (p2,  Epsilon, pS')
    , (pS,  Epsilon, pS2)
    , (pS2, NTE "A", p3)
    , (p3,  TE  "d", p4)
    , (p4,  Epsilon, pS')
    -- Submachine for A:
    , (pA,  Epsilon, pA1)
    , (pA1, TE  "a", p5)
    , (p5,  NTE "A", p6)
    , (p6,  Epsilon, pA')
    , (pA,  Epsilon, pA2)
    , (pA2, TE  "b", p7)
    , (p7,  Epsilon, pA')
    ]
  }

