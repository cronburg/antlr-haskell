module Main where

  import System.IO.Unsafe (unsafePerformIO)
  import Data.Monoid
  import Test.Framework
  import Test.Framework.Providers.HUnit
  import Test.Framework.Providers.QuickCheck2
  import Test.HUnit
  import Test.QuickCheck (Property, quickCheck, (==>))
  import qualified Test.QuickCheck.Monadic as TQM
  import Test.Text.ANTLR.Allstar.Grammar

  import Text.ANTLR.Allstar.ATN
  import Text.ANTLR.Allstar.Stacks (Stacks(..))
  import Text.ANTLR.Allstar
  import Control.Monad.State
  import Data.Set (Set(..), fromList)
  import qualified Data.Set as Set


  test_closure_01 = actual @?= expected
    where
      (actual,_) =
        runState (do
          ATN {_Δ = d} <-  getATN
          return $ closure d Set.empty (Start "C", 0, Empty))
          ( Parser { g = mattToolG})
      expected :: Set (Configuration String)
      expected   =
        fromList [(Start "A",0,Stacks (fromList [[Middle "C" 4 1]]))
                 ,(Start "C",0,Empty)
                 ,(Middle "A" 0 0,0,Stacks (fromList [[Middle "C" 4 1]]))
                 ,(Middle "A" 1 0,0,Stacks (fromList [[Middle "C" 4 1]]))
                 ,(Middle "C" 4 0,0,Empty)
                 ]
  test_move_01 = actual @?= expected
    where
      (actual,_) =
        runState
          (do
            ATN {_Δ = d} <-  getATN
            let epsilon_closure = closure d Set.empty (Start "C",0,Empty)
            move epsilon_closure "a"
          )
          ( Parser { g = mattToolG})
      expected :: Set (Configuration String)
      expected = fromList [(Middle "A" 0 1,0,Empty)
                          ,(Middle "A" 0 1,0,Stacks (fromList [[Middle "C" 4 1]]))
                          ,(Middle "A" 1 1,0,Empty)
                          ,(Middle "A" 1 1,0,Stacks (fromList [[Middle "C" 4 1]]))
                          ]

  test_conflict_01 = actual @?= expected
    where
      actual = getConflictSetsPerLoc $
                 fromList [(Middle "A" 0 1,2,Empty)
                          ,(Middle "A" 0 1,5,Empty)
                          ,(Start  "S"    ,0,Empty)
                          ]
      expected =
        fromList [ fromList [2,5]
                 , fromList [0] ]
  -- from the paper, pg 10
  test_conflict_02 = actual @?= expected
   where
     p = Middle "A" 0 1
     r = Start  "S"
     gamma   = Empty
     gamma'  = Stacks (fromList [[p]])
     gamma'' = Stacks (fromList [[r]])

     actual = getConflictSetsPerLoc $
                fromList [ (p,1,gamma)
                         , (p,2,gamma)
                         , (p,3,gamma)
                         , (p,1,gamma')
                         , (p,2,gamma')
                         , (r,2,gamma'')
                         ]
     expected =
       fromList [ fromList [1,2,3]
                , fromList [1,2]
                , fromList [2]]

  -- from the paper, pg 10
  test_prod_01 = actual @?= expected
   where
     p = Middle "A" 0 1
     r = Start  "S"
     gamma   = Empty
     gamma'  = Stacks (fromList [[p]])
     gamma'' = Stacks (fromList [[r]])

     actual = getProdSetsPerState $
                fromList [ (p,1,gamma)
                         , (p,2,gamma)
                         , (p,3,gamma)
                         , (p,1,gamma')
                         , (p,2,gamma')
                         , (r,2,gamma'')
                         ]
     expected =
       fromList [ fromList [1,2,3]
                , fromList [2]]

  main :: IO ()
  main = defaultMainWithOpts
    [ testCase "closure_01"  test_closure_01
    , testCase "move_01"     test_move_01
    , testCase "conflict_01" test_conflict_01
    , testCase "conflict_02" test_conflict_02
    , testCase "prod_01"     test_prod_01
    ] mempty
