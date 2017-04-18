module Main where
-- Allstar imports go here, e.g.:
-- import Text.Allstar.ATN (..)
import Text.ANTLR.Allstar.Stacks
import Data.Set.Monad (size, empty, fromList, (\\), Set)
import Data.List(nub)

import System.IO.Unsafe (unsafePerformIO)
import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck
  ( Property, quickCheck, (==>), Arbitrary(..)
  , elements
  )
import qualified Test.QuickCheck.Monadic as TQM

stacks :: Set [Int]
stacks = fromList
  [ [0, 1, 3, 7]
  , [0, 1, 4, 7]
  , [0, 2, 5, 7]
  , [0, 2, 6, 8]
  ]

gss :: Stacks Int
gss = Stacks stacks
--gss = Stacks.fromStacks stacks

gssExp =
  ( fromList  [ (7,3), (3,1), (1,0)
              , (7,4), (4,1), (1,0)
              , (7,5), (5,2), (2,0)
              , (8,6), (6,2), (2,0)
              ]
  , fromList  [0])

gssPopExp =
  ( fromList  [ (7,3), (3,1), (1,0)
              , (7,4), (4,1), (1,0)
              , (7,5), (5,2), (2,0)
              , (8,6), (6,2), (2,0)
              ]
  , fromList  [1,2])

test_wikiGSS = pop gss @?= gssPopExp

test_wikiGSS2 = ((fst.pop) gss \\ fst gssPopExp) @?= empty

data Idx = A | B | C | D | E | F | G | H | I | J
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Arbitrary Idx where
  arbitrary = elements [A .. maxBound :: Idx]

--distinct v = (length.nub) v == length v

--propFromTo :: [Idx] -> Property
--propFromTo v = distinct v ==> (fromStacks . toStacks) v == v

propToFrom :: Set [Idx] -> Property
propToFrom v = True ==> (toStacks . fromStacks) v == v

main :: IO ()
main = defaultMainWithOpts
  [ testCase "wikipedia_GSS" test_wikiGSS
  , testCase "wikipedia_GSS" test_wikiGSS2
-- TODO: make these work
--  , testProperty "fromToStack" propFromTo
--  , testProperty "toFromStack" propToFrom
  ] mempty

