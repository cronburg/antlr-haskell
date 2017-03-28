module Main where
import Test.Text.ANTLR.Allstar.Grammar
import Text.ANTLR.Allstar.Grammar
import Text.ANTLR.LR1
import Text.ANTLR.AST
import Text.ANTLR.Parser

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
    [ ((_I0, Icon "id"), Shift _I5)
    , ((_I0, Icon "("),  Shift _I4)
    , ((_I1, Icon "+"),  Shift _I6)
    , ((_I1, IconEOF),        Accept)
    , ((_I2, Icon "+"),  r2)
    , ((_I2, Icon "*"),  Shift _I7)
    , ((_I2, Icon ")"),  r2)
    , ((_I2, IconEOF),        r2)
    , ((_I3, Icon "+"),  r4)
    , ((_I3, Icon "*"),  r4)
    , ((_I3, Icon ")"),  r4)
    , ((_I3, IconEOF),        r4)
    , ((_I4, Icon "id"), Shift _I5)
    , ((_I4, Icon "("),  Shift _I4)
    , ((_I5, Icon "+"),  r6)
    , ((_I5, Icon "*"),  r6)
    , ((_I5, Icon ")"),  r6)
    , ((_I5, IconEOF),        r6)
    , ((_I6, Icon "id"), Shift _I5)
    , ((_I6, Icon "("),  Shift _I4)
    , ((_I7, Icon "id"), Shift _I5)
    , ((_I7, Icon "("),  Shift _I4)
    , ((_I8, Icon "+"),  Shift _I6)
    , ((_I8, Icon ")"),  Shift _I11)
    , ((_I9, Icon "+"),  r1)
    , ((_I9, Icon "*"),  Shift _I7)
    , ((_I9, Icon ")"),  r1)
    , ((_I9, IconEOF),        r1)
    , ((_I10, Icon "+"), r3)
    , ((_I10, Icon "*"), r3)
    , ((_I10, Icon ")"), r3)
    , ((_I10, IconEOF),       r3)
    , ((_I11, Icon "+"), r5)
    , ((_I11, Icon "*"), r5)
    , ((_I11, Icon ")"), r5)
    , ((_I11, IconEOF),       r5)
    ]

testLRRecognize =
  slrRecognize grm w0
  @?=
  True

testLRRecognize2 =
  slrRecognize grm (map Icon ["id", "*", "id", "+", "+"] ++ [IconEOF])
  @?=
  False

type LRAST = AST LR1NonTerminal LR1Terminal

action0 :: ParseEvent LRAST LR1NonTerminal LR1Terminal -> LRAST
action0 (TermE (Icon t))    = Leaf t
action0 (TermE IconEps)      = LeafEps
--action0 (TermE IconEOF)       = LeafIconEOF
action0 (NonTE (nt, ss, asts)) = AST nt ss asts

testLRParse =
  slrParse grm action0 w0
  @?=
  (Just $
    AST "E" [NT "E", T "+", NT "T"]
      [ AST "E" [NT "T"]
          [ AST "T" [NT "T", T "*", NT "F"]
              [ AST "T" [NT "F"] [AST "F" [T "id"] [Leaf "id"]]
              , Leaf "*"
              , AST "F" [T "id"] [Leaf "id"]
              ]
          ]
      , Leaf "+"
      , AST "T" [NT "F"] [AST "F" [T "id"] [Leaf "id"]]
      ])

testLRParse2 =
  slrParse grm action0 (map Icon ["id", "*", "id", "+", "+"] ++ [IconEOF])
  @?=
  Nothing

w0 = map Icon ["id", "*", "id", "+", "id"] ++ [IconEOF]

testLR1Table =
  lr1Table dragonBook455
  @?=
  lr1TableExp

lr1TableExp = M.fromList
  [ ((i0, Icon "c"), Shift i3)
  , ((i0, Icon "d"), Shift i4)
  , ((i1, IconEOF),       Accept)
  , ((i2, Icon "c"), Shift i6)
  , ((i2, Icon "d"), Shift i7)
  , ((i3, Icon "c"), Shift i3)
  , ((i3, Icon "d"), Shift i4)
  , ((i4, Icon "c"), r3')
  , ((i4, Icon "d"), r3')
  , ((i5, IconEOF),       r1')
  , ((i6, Icon "c"), Shift i6)
  , ((i6, Icon "d"), Shift i7)
  , ((i7, IconEOF),       r3')
  , ((i8, Icon "c"), r2')
  , ((i8, Icon "d"), r2')
  , ((i9, IconEOF),       r2')
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
  [ Item (Init   "S") [] [NT "S"]         IconEOF
  , Item (ItemNT "S") [] [NT "C", NT "C"] IconEOF
  , Item (ItemNT "C") [] [T "c", NT "C"]  (Icon "c")
  , Item (ItemNT "C") [] [T "c", NT "C"]  (Icon "d")
  , Item (ItemNT "C") [] [T "d"]          (Icon "c")
  , Item (ItemNT "C") [] [T "d"]          (Icon "d")
  ]

i1 = fromList [ Item (Init "S") [NT "S"] [] IconEOF ]

i2 = fromList
  [ Item (ItemNT "S") [NT "C"] [NT "C"]   IconEOF
  , Item (ItemNT "C") [] [T "c", NT "C"]  IconEOF
  , Item (ItemNT "C") [] [T "d"]          IconEOF
  ]

i3 = fromList
  [ Item (ItemNT "C") [T "c"] [NT "C"]    (Icon "c")
  , Item (ItemNT "C") [T "c"] [NT "C"]    (Icon "d")
  , Item (ItemNT "C") [] [T "c", NT "C"]  (Icon "c")
  , Item (ItemNT "C") [] [T "c", NT "C"]  (Icon "d")
  , Item (ItemNT "C") [] [T "d"]          (Icon "c")
  , Item (ItemNT "C") [] [T "d"]          (Icon "d")
  ]

i4 = fromList
  [ Item (ItemNT "C") [T "d"] [] (Icon "c")
  , Item (ItemNT "C") [T "d"] [] (Icon "d")
  ]

i5 = fromList [ Item (ItemNT "S") [NT "C", NT "C"] [] IconEOF ]

i6 = fromList
  [ Item (ItemNT "C") [T "c"] [NT "C"]    IconEOF
  , Item (ItemNT "C") [] [T "c", NT "C"]  IconEOF
  , Item (ItemNT "C") [] [T "d"]          IconEOF
  ]

i7 = fromList [ Item (ItemNT "C") [T "d"] [] IconEOF ]

i8 = fromList
  [ Item (ItemNT "C") [NT "C", T "c"] [] (Icon "c")
  , Item (ItemNT "C") [NT "C", T "c"] [] (Icon "d")
  ]

i9 = fromList [ Item (ItemNT "C") [NT "C", T "c"] [] IconEOF ]

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

