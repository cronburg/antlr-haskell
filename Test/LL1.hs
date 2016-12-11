module Main where
import Test.Text.ANTLR.Allstar.Grammar
import Text.ANTLR.Allstar.Grammar
import Text.ANTLR.LL1

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

grm = dragonBook428

termination = first grm [NT "E"] @?= first grm [NT "E"]

firstF = first grm [NT "F"] @?= fromList [Token "(", Token "id"]

noEps = first grm [NT "E"] @?= fromList [Token "(", Token "id"]

firstT' =
  first grm [NT "T'"]
  @?=
  fromList [Token "*", Eps']

foldEpsTest = foldWhileEpsilon union empty
  [ fromList [Token "(", Token "id"]
  , fromList [Token ")"]
  ]
  @?=
  fromList [Token "(", Token "id"]

firstAll =
  ( S.map ((\nt -> (nt, first grm [nt])) . NT) (ns grm)
    `union`
    S.map ((\t  -> (t,  first grm [t]))  . T)  (ts grm)
  )
  @?=
  fromList
    [ (NT "E",  fromList [Token "(", Token "id"])
    , (NT "E'", fromList [Token "+", Eps'])
    , (NT "F",  fromList [Token "(", Token "id"])
    , (NT "T",  fromList [Token "(", Token "id"])
    , (NT "T'", fromList [Token "*", Eps'])
    , (T "(",   fromList [Token "("])
    , (T ")",   fromList [Token ")"])
    , (T "*",   fromList [Token "*"])
    , (T "+",   fromList [Token "+"])
    , (T "id",  fromList [Token "id"])
    ]

followAll =
  S.map (\nt -> (NT nt, follow grm nt)) (ns grm)
  @?=
  fromList
    [ (NT "E",  fromList [Token ")", EOF])
    , (NT "E'", fromList [Token ")", EOF])
    , (NT "T",  fromList [Token ")", Token "+", EOF])
    , (NT "T'", fromList [Token ")", Token "+", EOF])
    , (NT "F",  fromList [Token ")", Token "*", Token "+", EOF])
    ]

parseTableTest =
  parseTable grm
  @?=
  M.fromList (map (\((a,b),c) -> ((a,b), S.singleton c))
    -- Figure 4.17 of dragon book:
    [ (("E",  Token "id"), [NT "T", NT "E'"])
    , (("E",  Token "("),  [NT "T", NT "E'"])
    , (("E'", Token "+"),  [T "+", NT "T", NT "E'"])
    , (("E'", Token ")"),  [Eps])
    , (("E'", EOF),       [Eps])
    , (("T",  Token "id"), [NT "F", NT "T'"])
    , (("T",  Token "("),  [NT "F", NT "T'"])
    , (("T'", Token "+"),  [Eps])
    , (("T'", Token "*"),  [T "*", NT "F", NT "T'"])
    , (("T'", Token ")"),  [Eps])
    , (("T'", EOF),       [Eps])
    , (("F",  Token "id"), [T "id"])
    , (("F",  Token "("),  [T "(", NT "E", T ")"])
    ])

data UAST =
  UAST 
    NonTerminal
    Symbols
    [Either UAST Terminal]
  deriving (Eq, Ord, Show)

action0 (nt, ss) = UAST nt ss

action1 prod trees = uPIO (print ("Act:", prod, trees)) `seq` action0 prod trees

dragonPredParse =
  (predictiveParse grm action1 $ map Token ["id", "+", "id", "*", "id"] ++ [EOF])
  @?=
  Just []

main :: IO ()
main = defaultMainWithOpts
  [ testCase "fold_epsilon" foldEpsTest
  , testCase "termination" termination
  , testCase "no_epsilon" noEps
  , testCase "firstF" firstF
  , testCase "firstT'" firstT'
  , testCase "firstAll" firstAll
  , testCase "followAll" followAll
  , testCase "dragonHasAllNonTerms" $ hasAllNonTerms grm @?= True
  , testCase "dragonHasAllTerms" $ hasAllTerms grm @?= True
  , testCase "dragonStartIsNonTerm" $ startIsNonTerm grm @?= True
  , testCase "dragonDistinctTermsNonTerms" $ distinctTermsNonTerms grm @?= True
  , testCase "dragonIsValid" $ validGrammar grm @?= True
  , testCase "dragonIsLL1" $ isLL1 grm @?= True
  , testCase "dragonParseTable" parseTableTest
  , testCase "dragonPredParse" dragonPredParse
  ] mempty

