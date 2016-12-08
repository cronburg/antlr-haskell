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
        runState
          (closure Set.empty (Start "C", 0, Empty))
          ( Parser { g = mattToolG})
      expected :: Set Configuration
      expected   =
        fromList [(Start "A",0,Stacks (fromList [[Middle "C" 4 1]]))
                 ,(Start "C",0,Empty)
                 ,(Middle "A" 0 0,0,Stacks (fromList [[Middle "C" 4 1]]))
                 ,(Middle "A" 1 0,0,Stacks (fromList [[Middle "C" 4 1]]))
                 ,(Middle "C" 4 0,0,Empty)
                 ]

  main :: IO ()
  main = defaultMainWithOpts
    [ testCase "closure_01" test_closure_01
    ] mempty
