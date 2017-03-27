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

iS = InputSymbol

type LR1Terminal = String
type LR1NonTerminal = String

newtype Item' = I' (Item () String String)
  deriving (Eq, Ord, Show)

instance Arbitrary Item' where
  arbitrary = (elements . map I' . S.toList . allSLRItems) grm

c' = slrClosure grm

propClosureClosure :: Set Item' -> Property
propClosureClosure items' = let items = S.map (\(I' is) -> is) items' in True ==>
  (c' . c') items == c' items

newtype Grammar' = G' (Grammar () String String)
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
    [ ((_I0, iS "id"), Shift _I5)
    , ((_I0, iS "("),  Shift _I4)
    , ((_I1, iS "+"),  Shift _I6)
    , ((_I1, EOF),        Accept)
    , ((_I2, iS "+"),  r2)
    , ((_I2, iS "*"),  Shift _I7)
    , ((_I2, iS ")"),  r2)
    , ((_I2, EOF),        r2)
    , ((_I3, iS "+"),  r4)
    , ((_I3, iS "*"),  r4)
    , ((_I3, iS ")"),  r4)
    , ((_I3, EOF),        r4)
    , ((_I4, iS "id"), Shift _I5)
    , ((_I4, iS "("),  Shift _I4)
    , ((_I5, iS "+"),  r6)
    , ((_I5, iS "*"),  r6)
    , ((_I5, iS ")"),  r6)
    , ((_I5, EOF),        r6)
    , ((_I6, iS "id"), Shift _I5)
    , ((_I6, iS "("),  Shift _I4)
    , ((_I7, iS "id"), Shift _I5)
    , ((_I7, iS "("),  Shift _I4)
    , ((_I8, iS "+"),  Shift _I6)
    , ((_I8, iS ")"),  Shift _I11)
    , ((_I9, iS "+"),  r1)
    , ((_I9, iS "*"),  Shift _I7)
    , ((_I9, iS ")"),  r1)
    , ((_I9, EOF),        r1)
    , ((_I10, iS "+"), r3)
    , ((_I10, iS "*"), r3)
    , ((_I10, iS ")"), r3)
    , ((_I10, EOF),       r3)
    , ((_I11, iS "+"), r5)
    , ((_I11, iS "*"), r5)
    , ((_I11, iS ")"), r5)
    , ((_I11, EOF),       r5)
    ]

testLRRecognize =
  slrRecognize grm w0
  @?=
  True

testLRRecognize2 =
  slrRecognize grm (map iS ["id", "*", "id", "+", "+"] ++ [EOF])
  @?=
  False

data UAST =
    ULeafEOF
  | ULeafEps
  | ULeaf LR1Terminal
  | UAST  LR1NonTerminal
          (Symbols LR1NonTerminal LR1Terminal)
          [UAST]
  deriving (Eq, Ord, Show)

action0 :: ParseEvent UAST LR1NonTerminal LR1Terminal -> UAST
action0 (InputSymbolE (InputSymbol t))    = ULeaf t
action0 (InputSymbolE Eps')      = ULeafEps
action0 (InputSymbolE EOF)       = ULeafEOF
action0 (NonTE (nt, ss, asts)) = UAST nt ss asts

testLRParse =
  slrParse grm action0 w0
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
  slrParse grm action0 (map iS ["id", "*", "id", "+", "+"] ++ [EOF])
  @?=
  Nothing

w0 = map iS ["id", "*", "id", "+", "id"] ++ [EOF]

testLR1Table =
  lr1Table dragonBook455
  @?=
  lr1TableExp

lr1TableExp = M.fromList
  [ ((i0, iS "c"), Shift i3)
  , ((i0, iS "d"), Shift i4)
  , ((i1, EOF),       Accept)
  , ((i2, iS "c"), Shift i6)
  , ((i2, iS "d"), Shift i7)
  , ((i3, iS "c"), Shift i3)
  , ((i3, iS "d"), Shift i4)
  , ((i4, iS "c"), r3')
  , ((i4, iS "d"), r3')
  , ((i5, EOF),       r1')
  , ((i6, iS "c"), Shift i6)
  , ((i6, iS "d"), Shift i7)
  , ((i7, EOF),       r3')
  , ((i8, iS "c"), r2')
  , ((i8, iS "d"), r2')
  , ((i9, EOF),       r2')
  ]

--r5 = Reduce ("F", Prod [T "(", NT "E", T ")"])
r1' = Reduce ("S", Prod [NT "C", NT "C"])
r2' = Reduce ("C", Prod [T "c", NT "C"])
r3' = Reduce ("C", Prod [T "d"])

testLR1Items =
  lr1Items dragonBook455
  @?=
  fromList [i0,i1,i2,i3,i4,i5,i6,i7,i8,i9]

-- page 262 of soft cover dragon book:
i0 = fromList
  [ Item (Init   "S") [] [NT "S"]         EOF
  , Item (ItemNT "S") [] [NT "C", NT "C"] EOF
  , Item (ItemNT "C") [] [T "c", NT "C"]  (iS "c")
  , Item (ItemNT "C") [] [T "c", NT "C"]  (iS "d")
  , Item (ItemNT "C") [] [T "d"]          (iS "c")
  , Item (ItemNT "C") [] [T "d"]          (iS "d")
  ]

i1 = fromList [ Item (Init "S") [NT "S"] [] EOF ]

i2 = fromList
  [ Item (ItemNT "S") [NT "C"] [NT "C"]   EOF
  , Item (ItemNT "C") [] [T "c", NT "C"]  EOF
  , Item (ItemNT "C") [] [T "d"]          EOF
  ]

i3 = fromList
  [ Item (ItemNT "C") [T "c"] [NT "C"]    (iS "c")
  , Item (ItemNT "C") [T "c"] [NT "C"]    (iS "d")
  , Item (ItemNT "C") [] [T "c", NT "C"]  (iS "c")
  , Item (ItemNT "C") [] [T "c", NT "C"]  (iS "d")
  , Item (ItemNT "C") [] [T "d"]          (iS "c")
  , Item (ItemNT "C") [] [T "d"]          (iS "d")
  ]

i4 = fromList
  [ Item (ItemNT "C") [T "d"] [] (iS "c")
  , Item (ItemNT "C") [T "d"] [] (iS "d")
  ]

i5 = fromList [ Item (ItemNT "S") [NT "C", NT "C"] [] EOF ]

i6 = fromList
  [ Item (ItemNT "C") [T "c"] [NT "C"]    EOF
  , Item (ItemNT "C") [] [T "c", NT "C"]  EOF
  , Item (ItemNT "C") [] [T "d"]          EOF
  ]

i7 = fromList [ Item (ItemNT "C") [T "d"] [] EOF ]

i8 = fromList
  [ Item (ItemNT "C") [NT "C", T "c"] [] (iS "c")
  , Item (ItemNT "C") [NT "C", T "c"] [] (iS "d")
  ]

i9 = fromList [ Item (ItemNT "C") [NT "C", T "c"] [] EOF ]

testLR1Parse =
  lr1Parse grm action0 w0
  @?=
  slrParse grm action0 w0

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
  , testCase "testLR1Parse" testLR1Parse
  , testCase "testLR1Items" testLR1Items
  , testCase "testLR1Table" testLR1Table
  ] mempty

