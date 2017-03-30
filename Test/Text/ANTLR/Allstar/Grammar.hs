module Test.Text.ANTLR.Allstar.Grammar where
import Data.Set (fromList, member, (\\), empty)
import Text.ANTLR.Allstar.Grammar

mattToolG = defaultGrammar
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

dragonBook428 = defaultGrammar
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

dragonBook41 = defaultGrammar
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

dragonBook455 = defaultGrammar
  { ns = fromList ["S", "C"]
  , ts = fromList ["c", "d"]
  , s0 = "S"
  , ps =  [ ("S", Prod [NT "C", NT "C"])
          , ("C", Prod [T "c", NT "C"])
          , ("C", Prod [T "d"])
          ]
  }

dumbGrammar :: Grammar
dumbGrammar = Grammar
  { ns = fromList ["S", "A", "B", "I", "D"]
  , ts = fromList ["1","2","3","+","-","*"]
  , s0 = "S"
  , gP = [ ("S", Prod [Left "A"])
         , ("S", Prod [Left "B"])
         , ("S", Prod [Left "D"])
         , ("A", Prod [Left "I", Right "+", Left "I"])
         , ("B", Prod [Left "I", Right "-", Left "I"])
         , ("I", Prod [Right "1"])
         , ("I", Prod [Right "2"])
         , ("I", Prod [Right "3"])
         , ("D", Prod [Left "I", Right "*", Left "I"])
         ]
  , ps = [(\_ -> True)]
  }

