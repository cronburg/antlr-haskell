module Test.Text.Allstar.Grammar where
import Data.Set (fromList)
import Text.Allstar.Grammar

mattToolG = defaultGrammar
  { ns = fromList ["A", "B", "C"]
  , ts = fromList ["a", "b", "c"]
  , s0 = "C"
  , ps =
          [ Production "A" $ Prod [T "a", T "b"]
          , Production "A" $ Prod [T "a"]
          , Production "B" $ Prod [NT "A", T "b"]
          , Production "B" $ Prod [T "b"]
          , Production "C" $ Prod [NT "A", NT "B", NT "C"]
          ]
  }

