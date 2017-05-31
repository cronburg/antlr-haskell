{-# LANGUAGE ExplicitForAll, DeriveAnyClass, DeriveGeneric, TypeFamilies
 , DeriveDataTypeable #-}
module Test.Text.ANTLR.Allstar.Grammar where
import Text.ANTLR.Set (fromList, member, (\\), empty, Generic(..), Hashable(..))
import Text.ANTLR.Allstar.Grammar
import Text.ANTLR.Pretty
import Data.Data (toConstr, Data(..))

data NS0 = A  | B  | C  deriving (Eq, Ord, Generic, Hashable, Bounded, Enum, Show, Data)
data TS0 = A_ | B_ | C_ deriving (Eq, Ord, Generic, Hashable, Bounded, Enum, Show, Data)
a = A_
b = B_
c = C_

-- TODO: boilerplate identity type classes for bounded enums
instance Ref NS0 where
  type Sym NS0 = NS0
  getSymbol = id
instance Ref TS0 where
  type Sym TS0 = TS0
  getSymbol = id
instance Prettify NS0 where prettify = rshow . toConstr
instance Prettify TS0 where prettify = rshow . toConstr
dG :: Grammar () NS0 TS0
dG = defaultGrammar C

mattToolG :: Grammar () NS0 TS0
mattToolG = dG
  { ns = fromList [A, B, C]
  , ts = fromList [a, b, c]
  , s0 = C
  , ps =
          [ Production A $ Prod Pass [T a, T b]
          , Production A $ Prod Pass [T a]
          , Production B $ Prod Pass [NT A, T b]
          , Production B $ Prod Pass [T b]
          , Production C $ Prod Pass [NT A, NT B, NT C]
          ]
  }

dG' :: Grammar () String String
dG' = defaultGrammar "A"

dragonBook428 :: Grammar () String String
dragonBook428 = dG'
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
dragonBook41 = dG'
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
dragonBook455 = dG'
  { ns = fromList ["S", "C"]
  , ts = fromList ["c", "d"]
  , s0 = "S"
  , ps =  [ Production "S" $ Prod Pass [NT "C", NT "C"]
          , Production "C" $ Prod Pass [T "c", NT "C"]
          , Production "C" $ Prod Pass [T "d"]
          ]
  }

dumbGrammar :: Grammar () String String
dumbGrammar = dG'
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

