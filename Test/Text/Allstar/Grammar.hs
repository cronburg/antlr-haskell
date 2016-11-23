module Test.Text.Allstar.Grammar where
import Text.Allstar.Grammar

mattToolG = defaultGrammar
  { n  = fromList "ABC"
  , t  = fromList "abc"
  , p  = [ Prod "A" [Right "a", Right "b"]
         , Prod "A" [Right "a"]
         , Prod "B" [Left "A", Right "b"]
         , Prod "B" [Right "b"]
         , Prod "C" [Left "A", Left "B", Left "C"]
         ]
  , s  = "C"
  }



