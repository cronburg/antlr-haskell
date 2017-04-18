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
          [ ("A", Prod [T "a", T "b"])
          , ("A", Prod [T "a"])
          , ("B", Prod [NT "A", T "b"])
          , ("B", Prod [T "b"])
          , ("C", Prod [NT "A", NT "B", NT "C"])
          ]
  }

dragonBook428 :: Grammar () String String
dragonBook428 = (defaultGrammar :: Grammar () String String)
  { ns = fromList ["E", "E'", "T", "T'", "F"]
  , ts = fromList ["+", "*", "(", ")", "id"]
  , s0 = "E"
  , ps = [ ("E",  Prod [NT "T", NT "E'"])
         , ("E'", Prod [T "+", NT "T", NT "E'"])
         , ("E'", Prod [Eps]) -- Implicitly epsilon
         , ("T",  Prod [NT "F", NT "T'"])
         , ("T'", Prod [T "*", NT "F", NT "T'"])
         , ("T'", Prod [Eps])
         , ("F",  Prod [T "(", NT "E", T ")"])
         , ("F",  Prod [T "id"])
         ]
  }

dragonBook41 :: Grammar () String String
dragonBook41 = (defaultGrammar :: Grammar () String String)
  { ns = fromList ["E'", "E", "T", "F"]
  , ts = fromList ["+", "*", "(", ")", "id"]
  , s0 = "E"
  , ps =  [ ("E", Prod [NT "E", T "+", NT "T"])
          , ("E", Prod [NT "T"])
          , ("T", Prod [NT "T", T "*", NT "F"])
          , ("T", Prod [NT "F"])
          , ("F", Prod [T "(", NT "E", T ")"])
          , ("F", Prod [T "id"])
          ]
  }

dragonBook455 :: Grammar () String String
dragonBook455 = (defaultGrammar :: Grammar () String String)
  { ns = fromList ["S", "C"]
  , ts = fromList ["c", "d"]
  , s0 = "S"
  , ps =  [ ("S", Prod [NT "C", NT "C"])
          , ("C", Prod [T "c", NT "C"])
          , ("C", Prod [T "d"])
          ]
  }

dumbGrammar :: Grammar () String String
dumbGrammar = (defaultGrammar :: Grammar () String String)
  { ns = fromList ["S", "A", "B", "I", "D"]
  , ts = fromList ["1","2","3","+","-","*"]
  , s0 = "S"
  , ps = [ ("S", Prod [NT "A"])
         , ("S", Prod [NT "B"])
         , ("S", Prod [NT "D"])
         , ("A", Prod [NT "I", T "+", NT "I"])
         , ("B", Prod [NT "I", T "-", NT "I"])
         , ("I", Prod [T "1"])
         , ("I", Prod [T "2"])
         , ("I", Prod [T "3"])
         , ("D", Prod [NT "I", T "*", NT "I"])
         ]
  --, us = [(\_ -> True)]
  }

