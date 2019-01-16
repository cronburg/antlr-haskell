{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeSynonymInstances #-}
module Main where
import Example.Grammar
import Text.ANTLR.Grammar
import Text.ANTLR.LR
import Text.ANTLR.Parser
import qualified Data.Text as T
import qualified Text.ANTLR.Lex.Tokenizer as T

import Text.ANTLR.Set (fromList, union, empty, Set(..), (\\), Hashable(..), Generic(..))
import qualified Text.ANTLR.Set as S
import qualified Text.ANTLR.MultiMap as M

import System.IO.Unsafe (unsafePerformIO)
import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit hiding ((@?=), assertEqual)
import Test.QuickCheck
--import Test.QuickCheck ( Property, quickCheck, (==>)
--  , elements, Arbitrary(..)
--  )
import qualified Test.QuickCheck.Monadic as TQM

import Text.ANTLR.HUnit
import Text.ANTLR.Pretty (pshow)
import qualified Debug.Trace as D

import qualified GLRInc
import qualified GLRPartial as GP

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
  deriving (Eq, Show, Generic, Hashable)

instance Arbitrary Item' where
  arbitrary = (elements . map I' . S.toList . allSLRItems) grm

instance (Eq a, Hashable a, Arbitrary a) => Arbitrary (Set a) where
  arbitrary = fmap S.fromList arbitrary
  shrink = map S.fromList . shrink . S.toList

c' = slrClosure grm

propClosureClosure :: Set Item' -> Property
propClosureClosure items' = let items = S.map (\(I' is) -> is) items' in True ==>
  (c' . c') items == c' items

newtype Grammar' = G' (Grammar () String String ())
  deriving (Eq, Show)

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
closedItems (G' g) = True ==> null (S.foldr union empty (slrItems g) \\ allSLRItems g)

closedItems0 =
  S.foldr union empty (slrItems grm) \\ allSLRItems grm
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

r1 = Reduce $ production "E" $ Prod Pass [NT "E", T "+", NT "T"]
r2 = Reduce $ production "E" $ Prod Pass [NT "T"]
r3 = Reduce $ production "T" $ Prod Pass [NT "T", T "*", NT "F"]
r4 = Reduce $ production "T" $ Prod Pass [NT "F"]
r5 = Reduce $ production "F" $ Prod Pass [T "(", NT "E", T ")"]
r6 = Reduce $ production "F" $ Prod Pass [T "id"]

-- Easier to debug when shown separately:
testSLRTable =
  M.size (slrTable grm
  `M.difference`
  testSLRExp)
  @?=
  0

testSLRTable2 =
  M.size (testSLRExp
  `M.difference`
  slrTable grm)
  @?=
  0

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
  slrRecognize grm ["id", "*", "id", "+", "+", ""]
  @?=
  False

type LRAST = AST LR1NonTerminal LR1Terminal

action0 :: ParseEvent LRAST LR1NonTerminal LR1Terminal -> LRAST
action0 (TermE "")              = LeafEps
action0 (TermE t)               = Leaf t
action0 (NonTE (nt, ss, asts))  = AST nt ss asts

testLRParse =
  slrParse grm action0 w0
  @?=
  (ResultAccept $
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
  isError (slrParse grm action0 ["id", "*", "id", "+", "+", "_"])
  @?=
  True

w0 = ["id", "*", "id", "+", "id", ""]

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

--r5 = Reduce ("F", Prod Pass [T "(", NT "E", T ")"])
r1' = Reduce $ production "S" $ Prod Pass [NT "C", NT "C"]
r2' = Reduce $ production "C" $ Prod Pass [T "c", NT "C"]
r3' = Reduce $ production "C" $ Prod Pass [T "d"]

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

getAST (ResultAccept ast) = ast
getAST _ = error "bad parse"

testLR1Parse =
  getAST (lr1Parse grm action0 w0)
  @?=
  getAST (slrParse grm action0 w0)

testPrettify = unsafePerformIO $ putStrLn $ T.unpack $ pshow testSLRExp

testGLRParse =
  glrParse grm action0 w0
  @?=
  (ResultSet $ S.fromList [ ResultAccept (
    AST "E" [NT "E",T "+",NT "T"]
      [AST "E" [NT "T"]
        [AST "T" [NT "T",T "*",NT "F"]
          [AST "T" [NT "F"]
            [AST "F" [T "id"] [Leaf "id"]]
          ,Leaf "*"
          ,AST "F" [T "id"]
            [Leaf "id"]]]
        ,Leaf "+"
        ,AST "T" [NT "F"]
          [AST "F" [T "id"] [Leaf "id"]]])])

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
  , testCase "testPrettify" (testPrettify @?= ())
  , testCase "testGLR" testGLRParse
  , testCase "test_GLRInc" GLRInc.test_GLRInc
  , testCase "test_GLRPartial" GP.test_GLRPartial
  ] mempty

