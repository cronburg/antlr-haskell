module Main where
import Test.Text.ANTLR.Allstar.Grammar
import Text.ANTLR.Allstar.Grammar
import Text.ANTLR.LR1

import Data.Set (fromList, union, empty, Set(..))
import qualified Data.Set as S
import qualified Data.Map.Strict as M

import System.IO.Unsafe (unsafePerformIO)
import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck ( Property, quickCheck, (==>)
  , elements, Arbitrary(..)
  )
import qualified Test.QuickCheck.Monadic as TQM

uPIO = unsafePerformIO

grm = dragonBook41

testClosure =
  closure grm (S.singleton $ Item (Init "E") [] [NT "E"])
  @?=
  fromList
    [ Item (Init "E")   [] [NT "E"]
    , Item (ItemNT "E") [] [NT "E", T "+", NT "T"]
    , Item (ItemNT "E") [] [NT "T"]
    , Item (ItemNT "T") [] [NT "T", T "*", NT "F"]
    , Item (ItemNT "T") [] [NT "F"]
    , Item (ItemNT "F") [] [T "(", NT "E", T ")"]
    , Item (ItemNT "F") [] [T "id"]
    ]

testKernel =
  kernel (closure grm (S.singleton $ Item (Init "E") [] [NT "E"]))
  @?=
  fromList
    [Item (Init "E") [] [NT "E"]]

instance Arbitrary Item where
  arbitrary = (elements . S.toList . allItems) grm

c' = closure grm

propClosureClosure :: Set Item -> Property
propClosureClosure items = True ==>
  (c' . c') items == c' items 

main :: IO ()
main = defaultMainWithOpts
  [ testCase "closure" testClosure
  , testCase "kernel"  testKernel
  , testProperty "closure-closure" propClosureClosure
  ] mempty

