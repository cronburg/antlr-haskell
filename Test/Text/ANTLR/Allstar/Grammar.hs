{-# LANGUAGE ExplicitForAll #-}
module Test.Text.ANTLR.Allstar.Grammar where
import Text.ANTLR.Set (fromList, member, (\\), empty)
import Text.ANTLR.Allstar.Grammar

mattToolG :: Grammar () String String
mattToolG = (defaultGrammar :: Grammar () String String)
  { ns = fromList ["A", "B", "C"]
  , ts = fromList ["a", "b", "c"]
  , s0 = "C"
  , ps =
          [ Production "A" $ Prod Pass [T "a", T "b"]
          , Production "A" $ Prod Pass [T "a"]
          , Production "B" $ Prod Pass [NT "A", T "b"]
          , Production "B" $ Prod Pass [T "b"]
          , Production "C" $ Prod Pass [NT "A", NT "B", NT "C"]
          ]
  }

dragonBook428 :: Grammar () String String
dragonBook428 = (defaultGrammar :: Grammar () String String)
  { ns = fromList ["E", "E'", "T", "T'", "F"]
  , ts = fromList ["+", "*", "(", ")", "id"]
  , s0 = "E"
  , ps = [ Production "E"  $ Prod Pass [NT "T", NT "E'"]
         , Production "E'" $ Prod Pass [T "+", NT "T", NT "E'"]
         , Production "E'" $ Prod Pass [Eps] -- Implicitly epsilon
         , Production "T"  $ Prod Pass [NT "F", NT "T'"]
         , Production "T'" $ Prod Pass [T "*", NT "F", NT "T'"]
         , Production "T'" $ Prod Pass [Eps]
         , Production "F"  $ Prod Pass [T "(", NT "E", T ")"]
         , Production "F"  $ Prod Pass [T "id"]
         ]
  }

dragonBook41 :: Grammar () String String
dragonBook41 = (defaultGrammar :: Grammar () String String)
  { ns = fromList ["E'", "E", "T", "F"]
  , ts = fromList ["+", "*", "(", ")", "id"]
  , s0 = "E"
  , ps =  [ Production "E" $ Prod Pass [NT "E", T "+", NT "T"]
          , Production "E" $ Prod Pass [NT "T"]
          , Production "T" $ Prod Pass [NT "T", T "*", NT "F"]
          , Production "T" $ Prod Pass [NT "F"]
          , Production "F" $ Prod Pass [T "(", NT "E", T ")"]
          , Production "F" $ Prod Pass [T "id"]
          ]
  }

dragonBook455 :: Grammar () String String
dragonBook455 = (defaultGrammar :: Grammar () String String)
  { ns = fromList ["S", "C"]
  , ts = fromList ["c", "d"]
  , s0 = "S"
  , ps =  [ Production "S" $ Prod Pass [NT "C", NT "C"]
          , Production "C" $ Prod Pass [T "c", NT "C"]
          , Production "C" $ Prod Pass [T "d"]
          ]
  }

dumbGrammar :: Grammar () String String
dumbGrammar = (defaultGrammar :: Grammar () String String)
  { ns = fromList ["S", "A", "B", "I", "D"]
  , ts = fromList ["1","2","3","+","-","*"]
  , s0 = "S"
  , ps = [ Production "S" $ Prod Pass [NT "A"]
         , Production "S" $ Prod Pass [NT "B"]
         , Production "S" $ Prod Pass [NT "D"]
         , Production "A" $ Prod Pass [NT "I", T "+", NT "I"]
         , Production "B" $ Prod Pass [NT "I", T "-", NT "I"]
         , Production "I" $ Prod Pass [T "1"]
         , Production "I" $ Prod Pass [T "2"]
         , Production "I" $ Prod Pass [T "3"]
         , Production "D" $ Prod Pass [NT "I", T "*", NT "I"]
         ]
  --, us = [(\_ -> True)]
  }

