module Main where
import Test.Text.ANTLR.Allstar.Grammar
import Text.ANTLR.Allstar.Grammar
import Text.ANTLR.LR1

import Data.Set (fromList, union, empty, Set(..), (\\))
import qualified Data.Set as S
import qualified Data.Map.Strict as M

import System.IO.Unsafe (unsafePerformIO)
import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck
--import Test.QuickCheck ( Property, quickCheck, (==>)
--  , elements, Arbitrary(..)
--  )
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

newtype Item' = I' Item
  deriving (Eq, Ord, Show)

instance Arbitrary Item' where
  arbitrary = (elements . map I' . S.toList . allItems) grm

c' = closure grm

propClosureClosure :: Set Item' -> Property
propClosureClosure items' = let items = S.map (\(I' is) -> is) items' in True ==>
  (c' . c') items == c' items 

newtype Grammar' = G' (Grammar ())
  deriving (Eq, Ord, Show)

instance Arbitrary Grammar' where
  arbitrary = return $ G' grm
{-
  arbitrary = do
    (uPIO $ print "damnit") `seq` return ()
    i <- elements [1..10]
    j <- elements [1..10]
    ns' <- infiniteList :: Gen [NonTerminal]
    ts' <- infiniteList :: Gen [Terminal]
    let ns = take i ns'
    let ts = take j ts'
    s0 <- elements ns
    let g = defaultGrammar {ns = fromList ns, ts = fromList ts, s0 = s0}
    let prod = do
          lhs <- elements ns
          rhs <- listOf (elements $ S.toList $ symbols g)
          return (lhs, Prod rhs)
    ps <- suchThat (listOf1 prod) (\ps -> validGrammar $ g { ps = ps })
    (uPIO $ print $ G' $ g { ps = ps }) `seq` return ()
    return $ G' $ g { ps = ps }
-}

closedItems :: Grammar' -> Property
closedItems (G' g) = True ==> null (S.fold union empty (items g) \\ allItems g)

closedItems0 =
  S.fold union empty (items grm) \\ allItems grm
  @?=
  empty

testItems =
  items grm
  @?=
  fromList
    -- I_0
    [ fromList  [ Item (Init "E") [] [NT "E"]
                , Item (ItemNT "E") [] [NT "E",T "+",NT "T"]
                , Item (ItemNT "E") [] [NT "T"]
                , Item (ItemNT "F") [] [T "(",NT "E",T ")"]
                , Item (ItemNT "F") [] [T "id"]
                , Item (ItemNT "T") [] [NT "F"]
                , Item (ItemNT "T") [] [NT "T",T "*",NT "F"]]
    -- I_1
    , fromList  [ Item (Init "E") [NT "E"] []
                , Item (ItemNT "E") [NT "E"] [T "+",NT "T"]]
    -- I_4
    , fromList  [ Item (ItemNT "E") [] [NT "E",T "+",NT "T"]
                , Item (ItemNT "E") [] [NT "T"]
                , Item (ItemNT "F") [] [T "(",NT "E",T ")"]
                , Item (ItemNT "F") [] [T "id"]
                , Item (ItemNT "F") [T "("] [NT "E",T ")"]
                , Item (ItemNT "T") [] [NT "F"]
                , Item (ItemNT "T") [] [NT "T",T "*",NT "F"]]
    -- I_8
    , fromList  [ Item (ItemNT "E") [NT "E"] [T "+",NT "T"]
                , Item (ItemNT "F") [NT "E",T "("] [T ")"]]
    -- I_2
    , fromList  [ Item (ItemNT "E") [NT "T"] []
                , Item (ItemNT "T") [NT "T"] [T "*",NT "F"]]
    -- I_9
    , fromList  [ Item (ItemNT "E") [NT "T",T "+",NT "E"] []
                , Item (ItemNT "T") [NT "T"] [T "*",NT "F"]]
    -- I_6
    , fromList  [ Item (ItemNT "E") [T "+",NT "E"] [NT "T"]
                , Item (ItemNT "F") [] [T "(",NT "E",T ")"]
                , Item (ItemNT "F") [] [T "id"]
                , Item (ItemNT "T") [] [NT "F"]
                , Item (ItemNT "T") [] [NT "T",T "*",NT "F"]]
    -- I_7
    , fromList  [ Item (ItemNT "F") [] [T "(",NT "E",T ")"]
                , Item (ItemNT "F") [] [T "id"]
                , Item (ItemNT "T") [T "*",NT "T"] [NT "F"]]
    -- I_11
    , fromList  [ Item (ItemNT "F") [T ")",NT "E",T "("] []]
    -- I_5
    , fromList  [ Item (ItemNT "F") [T "id"] []]
    -- I_3
    , fromList  [ Item (ItemNT "T") [NT "F"] []]
    -- I_10
    , fromList  [ Item (ItemNT "T") [NT "F",T "*",NT "T"] []]
    ]

main :: IO ()
main = defaultMainWithOpts
  [ testCase "closure" testClosure
  , testCase "kernel"  testKernel
  , testProperty "closure-closure" propClosureClosure
  , testCase "items" testItems
  , testCase "closedItems0" closedItems0
  , testProperty  "closedItems" closedItems
  ] mempty

