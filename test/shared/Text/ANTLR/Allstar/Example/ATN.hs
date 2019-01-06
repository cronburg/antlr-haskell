module Text.ANTLR.Allstar.Example.ATN where

import Text.ANTLR.Grammar
import Text.ANTLR.Allstar.ATN
import Text.ANTLR.Example.Grammar
import Text.ANTLR.Set (fromList, union)
import System.IO.Unsafe (unsafePerformIO)

-- This is the Grammar from page 6 of the
-- 'Adaptive LL(*) Parsing: The Power of Dynamic Analysis'
-- paper, namely the expected transitions based on Figures
-- 5 through 8:
paperATNGrammar = (defaultGrammar "C" :: Grammar () String String ())
  { ns = fromList ["S", "A"]
  , ts = fromList ["a", "b", "c", "d"]
  , s0 = "C"
  , ps =
          [ production "S" $ Prod Pass [NT "A", T "c"]
          , production "S" $ Prod Pass [NT "A", T "d"]
          , production "A" $ Prod Pass [T "a", NT "A"]
          , production "A" $ Prod Pass [T "b"]
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

--always _ = True
--never  _ = False

addPredicates = paperATNGrammar
  { ps =
    ps paperATNGrammar ++
    [ production "A" $ Prod (Sem (Predicate "always"  ()))    [T "a"]
    , production "A" $ Prod (Sem (Predicate "never"   ()))     []
    , production "A" $ Prod (Sem (Predicate "always2" ()))   [NT "A", T "a"]
    ]
  }

(pX,pY,pZ) = (Start "A", Start "A", Start "A")
pX1 = Middle "A" 4 2
pX2 = Middle "A" 4 0
pX3 = Middle "A" 4 1
pX4 = Accept "A"

pY1 = Middle "A" 5 1
pY2 = Middle "A" 5 0
pY3 = Accept "A"

pZ1 = Middle "A" 6 3
pZ2 = Middle "A" 6 0
pZ3 = Middle "A" 6 1
pZ4 = Middle "A" 6 2
pZ5 = Accept "A"

exp_addPredicates = ATN
  { _Δ = union (_Δ exp_paperATN) $ fromList
    [ (pX,  Epsilon, pX1)
    , (pX1, PE $ Predicate "always" (), pX2)
    , (pX2, TE "a", pX3)
    , (pX3, Epsilon, pX4)

    , (pY, Epsilon, pY1)
    , (pY1, PE $ Predicate "never" (), pY2)
    , (pY2, Epsilon, pY3)

    , (pZ, Epsilon, pZ1)
    , (pZ1, PE $ Predicate "always2" (), pZ2)
    , (pZ2, NTE "A", pZ3)
    , (pZ3, TE "a", pZ4)
    , (pZ4, Epsilon, pZ5)
    ]
  }

fireZeMissiles state = seq
  (unsafePerformIO $ putStrLn "Missiles fired.")
  undefined

addMutators = addPredicates
  { ps = ps addPredicates ++
    [ production "A" $ Prod (Action (Mutator "fireZeMissiles" ())) []
    , production "S" $ Prod (Action (Mutator "identity"       ())) []
    ]
  }

exp_addMutators = ATN
  { _Δ = union (_Δ exp_addPredicates) $ fromList
    [ (Start "A", Epsilon, Middle "A" 7 0)
    , (Middle "A" 7 0, ME $ Mutator "fireZeMissiles" (), Accept "A")
    , (Start "S", Epsilon, Middle "S" 8 0)
    , (Middle "S" 8 0, ME $ Mutator "identity" (), Accept "S")
    ]
  }


