{-# LANGUAGE ExplicitForAll #-}
module Test.Text.ANTLR.Allstar.Grammar where
import Data.Set.Monad (fromList, member, (\\), empty)
import Text.ANTLR.Allstar.Grammar

mattToolG :: Grammar () String String
mattToolG = (defaultGrammar :: Grammar () String String)
  { ns = fromList ["A", "B", "C"]
  , ts = fromList ["a", "b", "c"]
  , s0 = "C"
  , ps =
          [ ("A", Prod Pass [T "a", T "b"])
          , ("A", Prod Pass [T "a"])
          , ("B", Prod Pass [NT "A", T "b"])
          , ("B", Prod Pass [T "b"])
          , ("C", Prod Pass [NT "A", NT "B", NT "C"])
          ]
  }

dragonBook428 :: Grammar () String String
dragonBook428 = (defaultGrammar :: Grammar () String String)
  { ns = fromList ["E", "E'", "T", "T'", "F"]
  , ts = fromList ["+", "*", "(", ")", "id"]
  , s0 = "E"
  , ps = [ ("E",  Prod Pass [NT "T", NT "E'"])
         , ("E'", Prod Pass [T "+", NT "T", NT "E'"])
         , ("E'", Prod Pass [Eps]) -- Implicitly epsilon
         , ("T",  Prod Pass [NT "F", NT "T'"])
         , ("T'", Prod Pass [T "*", NT "F", NT "T'"])
         , ("T'", Prod Pass [Eps])
         , ("F",  Prod Pass [T "(", NT "E", T ")"])
         , ("F",  Prod Pass [T "id"])
         ]
  }

dragonBook41 :: Grammar () String String
dragonBook41 = (defaultGrammar :: Grammar () String String)
  { ns = fromList ["E'", "E", "T", "F"]
  , ts = fromList ["+", "*", "(", ")", "id"]
  , s0 = "E"
  , ps =  [ ("E", Prod Pass [NT "E", T "+", NT "T"])
          , ("E", Prod Pass [NT "T"])
          , ("T", Prod Pass [NT "T", T "*", NT "F"])
          , ("T", Prod Pass [NT "F"])
          , ("F", Prod Pass [T "(", NT "E", T ")"])
          , ("F", Prod Pass [T "id"])
          ]
  }

dragonBook455 :: Grammar () String String
dragonBook455 = (defaultGrammar :: Grammar () String String)
  { ns = fromList ["S", "C"]
  , ts = fromList ["c", "d"]
  , s0 = "S"
  , ps =  [ ("S", Prod Pass [NT "C", NT "C"])
          , ("C", Prod Pass [T "c", NT "C"])
          , ("C", Prod Pass [T "d"])
          ]
  }

dumbGrammar :: Grammar () String String
dumbGrammar = (defaultGrammar :: Grammar () String String)
  { ns = fromList ["S", "A", "B", "I", "D"]
  , ts = fromList ["1","2","3","+","-","*"]
  , s0 = "S"
  , ps = [ ("S", Prod Pass [NT "A"])
         , ("S", Prod Pass [NT "B"])
         , ("S", Prod Pass [NT "D"])
         , ("A", Prod Pass [NT "I", T "+", NT "I"])
         , ("B", Prod Pass [NT "I", T "-", NT "I"])
         , ("I", Prod Pass [T "1"])
         , ("I", Prod Pass [T "2"])
         , ("I", Prod Pass [T "3"])
         , ("D", Prod Pass [NT "I", T "*", NT "I"])
         ]
  --, us = [(\_ -> True)]
  }

