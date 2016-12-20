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

slrItem x y z = Item x y z ()

testClosure =
  slrClosure grm (S.singleton $ slrItem (Init "E") [] [NT "E"])
  @?=
  fromList
    [ slrItem (Init "E")   [] [NT "E"]
    , slrItem (ItemNT "E") [] [NT "E", T "+", NT "T"]
    , slrItem (ItemNT "E") [] [NT "T"]
    , slrItem (ItemNT "T") [] [NT "T", T "*", NT "F"]
    , slrItem (ItemNT "T") [] [NT "F"]
    , slrItem (ItemNT "F") [] [T "(", NT "E", T ")"]
    , slrItem (ItemNT "F") [] [T "id"]
    ]

testKernel =
  kernel (slrClosure grm (S.singleton $ slrItem (Init "E") [] [NT "E"]))
  @?=
  fromList
    [ slrItem (Init "E") [] [NT "E"] ]

newtype Item' = I' (Item ())
  deriving (Eq, Ord, Show)

instance Arbitrary Item' where
  arbitrary = (elements . map I' . S.toList . allSLRItems) grm

c' = slrClosure grm

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
closedItems (G' g) = True ==> null (S.fold union empty (slrItems g) \\ allSLRItems g)

closedItems0 =
  S.fold union empty (slrItems grm) \\ allSLRItems grm
  @?=
  empty

testItems =
  slrItems grm
  @?=
  fromList [_I0, _I1, _I2, _I3, _I4, _I5, _I6, _I7, _I8, _I9, _I10, _I11]

_I0 = fromList  [ slrItem (Init "E") [] [NT "E"]
                , slrItem (ItemNT "E") [] [NT "E",T "+",NT "T"]
                , slrItem (ItemNT "E") [] [NT "T"]
                , slrItem (ItemNT "F") [] [T "(",NT "E",T ")"]
                , slrItem (ItemNT "F") [] [T "id"]
                , slrItem (ItemNT "T") [] [NT "F"]
                , slrItem (ItemNT "T") [] [NT "T",T "*",NT "F"]]
_I1 = fromList  [ slrItem (Init "E") [NT "E"] []
                , slrItem (ItemNT "E") [NT "E"] [T "+",NT "T"]]
_I4 = fromList  [ slrItem (ItemNT "E") [] [NT "E",T "+",NT "T"]
                , slrItem (ItemNT "E") [] [NT "T"]
                , slrItem (ItemNT "F") [] [T "(",NT "E",T ")"]
                , slrItem (ItemNT "F") [] [T "id"]
                , slrItem (ItemNT "F") [T "("] [NT "E",T ")"]
                , slrItem (ItemNT "T") [] [NT "F"]
                , slrItem (ItemNT "T") [] [NT "T",T "*",NT "F"]]
_I8 = fromList  [ slrItem (ItemNT "E") [NT "E"] [T "+",NT "T"]
                , slrItem (ItemNT "F") [NT "E",T "("] [T ")"]]
_I2 = fromList  [ slrItem (ItemNT "E") [NT "T"] []
                , slrItem (ItemNT "T") [NT "T"] [T "*",NT "F"]]
_I9 = fromList  [ slrItem (ItemNT "E") [NT "T",T "+",NT "E"] []
                , slrItem (ItemNT "T") [NT "T"] [T "*",NT "F"]]
_I6 = fromList  [ slrItem (ItemNT "E") [T "+",NT "E"] [NT "T"]
                , slrItem (ItemNT "F") [] [T "(",NT "E",T ")"]
                , slrItem (ItemNT "F") [] [T "id"]
                , slrItem (ItemNT "T") [] [NT "F"]
                , slrItem (ItemNT "T") [] [NT "T",T "*",NT "F"]]
_I7 = fromList  [ slrItem (ItemNT "F") [] [T "(",NT "E",T ")"]
                , slrItem (ItemNT "F") [] [T "id"]
                , slrItem (ItemNT "T") [T "*",NT "T"] [NT "F"]]
_I11 = fromList  [ slrItem (ItemNT "F") [T ")",NT "E",T "("] []]
_I5  = fromList  [ slrItem (ItemNT "F") [T "id"] []]
_I3  = fromList  [ slrItem (ItemNT "T") [NT "F"] []]
_I10 = fromList  [ slrItem (ItemNT "T") [NT "F",T "*",NT "T"] []]

r1 = Reduce ("E", Prod [NT "E", T "+", NT "T"])
r2 = Reduce ("E", Prod [NT "T"])
r3 = Reduce ("T", Prod [NT "T", T "*", NT "F"])
r4 = Reduce ("T", Prod [NT "F"])
r5 = Reduce ("F", Prod [T "(", NT "E", T ")"])
r6 = Reduce ("F", Prod [T "id"])

-- Easier to debug when shown separately:
testSLRTable =
  (slrTable grm
  `M.difference`
  testSLRExp)
  @?=
  M.empty
testSLRTable2 =
  (testSLRExp 
  `M.difference`
  slrTable grm)
  @?=
  M.empty

testSLRTable3 = 
  slrTable grm
  @?=
  testSLRExp

testSLRExp = M.fromList
    [ ((_I0, Token "id"), Shift _I5)
    , ((_I0, Token "("),  Shift _I4)
    , ((_I1, Token "+"),  Shift _I6)
    , ((_I1, EOF),        Accept)
    , ((_I2, Token "+"),  r2)
    , ((_I2, Token "*"),  Shift _I7)
    , ((_I2, Token ")"),  r2)
    , ((_I2, EOF),        r2)
    , ((_I3, Token "+"),  r4)
    , ((_I3, Token "*"),  r4)
    , ((_I3, Token ")"),  r4)
    , ((_I3, EOF),        r4)
    , ((_I4, Token "id"), Shift _I5)
    , ((_I4, Token "("),  Shift _I4)
    , ((_I5, Token "+"),  r6)
    , ((_I5, Token "*"),  r6)
    , ((_I5, Token ")"),  r6)
    , ((_I5, EOF),        r6)
    , ((_I6, Token "id"), Shift _I5)
    , ((_I6, Token "("),  Shift _I4)
    , ((_I7, Token "id"), Shift _I5)
    , ((_I7, Token "("),  Shift _I4)
    , ((_I8, Token "+"),  Shift _I6)
    , ((_I8, Token ")"),  Shift _I11)
    , ((_I9, Token "+"),  r1)
    , ((_I9, Token "*"),  Shift _I7)
    , ((_I9, Token ")"),  r1)
    , ((_I9, EOF),        r1)
    , ((_I10, Token "+"), r3)
    , ((_I10, Token "*"), r3)
    , ((_I10, Token ")"), r3)
    , ((_I10, EOF),       r3)
    , ((_I11, Token "+"), r5)
    , ((_I11, Token "*"), r5)
    , ((_I11, Token ")"), r5)
    , ((_I11, EOF),       r5)
    ]

testLRRecognize =
  slrRecognize grm (map Token ["id", "*", "id", "+", "id"] ++ [EOF])
  @?=
  True

testLRRecognize2 =
  slrRecognize grm (map Token ["id", "*", "id", "+", "+"] ++ [EOF])
  @?=
  False

data UAST =
    ULeafEOF
  | ULeafEps
  | ULeaf Terminal
  | UAST  NonTerminal
          Symbols
          [UAST]
  deriving (Eq, Ord, Show)

action0 :: ParseEvent UAST -> UAST
action0 (TokenE (Token t)) = ULeaf t
action0 (TokenE Eps')      = ULeafEps
action0 (TokenE EOF)       = ULeafEOF
action0 (NonTE (nt, ss, asts)) = UAST nt ss asts

testLRParse =
  slrParse grm action0 (map Token ["id", "*", "id", "+", "id"] ++ [EOF])
  @?=
  (Just $
    UAST "E" [NT "E", T "+", NT "T"]
      [ UAST "E" [NT "T"]
          [ UAST "T" [NT "T", T "*", NT "F"]
              [ UAST "T" [NT "F"] [UAST "F" [T "id"] [ULeaf "id"]]
              , ULeaf "*"
              , UAST "F" [T "id"] [ULeaf "id"]
              ]
          ]
      , ULeaf "+"
      , UAST "T" [NT "F"] [UAST "F" [T "id"] [ULeaf "id"]]
      ])

testLRParse2 =
  slrParse grm action0 (map Token ["id", "*", "id", "+", "+"] ++ [EOF])
  @?=
  Nothing

main :: IO ()
main = defaultMainWithOpts
  [ testCase "closure" testClosure
  , testCase "kernel"  testKernel
  , testProperty "closure-closure" propClosureClosure
  , testCase "items" testItems
  , testCase "closedItems0" closedItems0
  , testProperty  "closedItems" closedItems
  , testCase "slrTable" testSLRTable
  , testCase "slrTable2" testSLRTable2
  , testCase "slrTable3" testSLRTable3
  , testCase "testLRRecognize" testLRRecognize
  , testCase "testLRRecognize2" testLRRecognize2
  , testCase "testLRParse" testLRParse
  , testCase "testLRParse2" testLRParse2
  ] mempty

