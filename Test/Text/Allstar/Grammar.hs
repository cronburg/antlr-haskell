module Test.Text.Allstar.Grammar where
import Data.Set (fromList)
import Text.Allstar.Grammar

mattToolG = defaultGrammar
  { ns = fromList "ABC"
  , ts = fromList "abc"
  , s0 = "C"
  , ps = [ Prod "A" [T "a", T "b"]
         , Prod "A" [T "a"]
         , Prod "B" [NT "A", T "b"]
         , Prod "B" [T "b"]
         , Prod "C" [NT "A", NT "B", NT "C"]
         ]
  }

