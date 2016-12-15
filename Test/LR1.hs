module Main where
import Test.Text.ANTLR.Allstar.Grammar
import Text.ANTLR.Allstar.Grammar
import Text.ANTLR.LR1

import Data.Set (fromList, union, empty)
import qualified Data.Set as S
import qualified Data.Map.Strict as M

import System.IO.Unsafe (unsafePerformIO)
import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck (Property, quickCheck, (==>))
import qualified Test.QuickCheck.Monadic as TQM

uPIO = unsafePerformIO

grm = dragonBook41

testClosure =
  closure grm (S.singleton $ Item "E'" [] [NT "E"])
  @?=
  fromList
    [ Item "E'" [] [NT "E"]
    , Item "E"  [] [NT "E", T "+", NT "T"]
    , Item "E"  [] [NT "T"]
    , Item "T"  [] [NT "T", T "*", NT "F"]
    , Item "T"  [] [NT "F"]
    , Item "F"  [] [T "(", NT "E", T ")"]
    , Item "F"  [] [T "id"]
    ]

main :: IO ()
main = defaultMainWithOpts
  [ testCase "closure" testClosure
  ] mempty

