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

grm = dragonBook428

termination = first grm [NT "E"] @?= first grm [NT "E"]

firstF = first grm [NT "F"] @?= fromList [Term "(", Term "id"]

noEps = first grm [NT "E"] @?= fromList [Term "(", Term "id"]

firstT' =
  first grm [NT "T'"]
  @?=
  fromList [Term "*", Eps']

foldEpsTest = foldWhileEpsilon union empty
  [ fromList [Term "(", Term "id"]
  , fromList [Term ")"]
  ]
  @?=
  fromList [Term "(", Term "id"]

firstAll =
  ( S.map ((\nt -> (nt, first grm [nt])) . NT) (ns grm)
    `union`
    S.map ((\t  -> (t,  first grm [t]))  . T)  (ts grm)
  )
  @?=
  fromList
    [ (NT "E",  fromList [Term "(", Term "id"])
    , (NT "E'", fromList [Term "+", Eps'])
    , (NT "F",  fromList [Term "(", Term "id"])
    , (NT "T",  fromList [Term "(", Term "id"])
    , (NT "T'", fromList [Term "*", Eps'])
    , (T "(",   fromList [Term "("])
    , (T ")",   fromList [Term ")"])
    , (T "*",   fromList [Term "*"])
    , (T "+",   fromList [Term "+"])
    , (T "id",  fromList [Term "id"])
    ]

followAll =
  S.map (\nt -> (NT nt, follow grm nt)) (ns grm)
  @?=
  fromList
    [ (NT "E",  fromList [Term ")", EOF])
    , (NT "E'", fromList [Term ")", EOF])
    , (NT "T",  fromList [Term ")", Term "+", EOF])
    , (NT "T'", fromList [Term ")", Term "+", EOF])
    , (NT "F",  fromList [Term ")", Term "*", Term "+", EOF])
    ]

parseTableTest =
  parseTable grm
  @?=
  M.fromList (map (\((a,b),c) -> ((a,b), S.singleton c))
    -- Figure 4.17 of dragon book:
    [ (("E",  Term "id"), [NT "T", NT "E'"])
    , (("E",  Term "("),  [NT "T", NT "E'"])
    , (("E'", Term "+"),  [T "+", NT "T", NT "E'"])
    , (("E'", Term ")"),  [Eps])
    , (("E'", EOF),       [Eps])
    , (("T",  Term "id"), [NT "F", NT "T'"])
    , (("T",  Term "("),  [NT "F", NT "T'"])
    , (("T'", Term "+"),  [Eps])
    , (("T'", Term "*"),  [T "*", NT "F", NT "T'"])
    , (("T'", Term ")"),  [Eps])
    , (("T'", EOF),       [Eps])
    , (("F",  Term "id"), [T "id"])
    , (("F",  Term "("),  [T "(", NT "E", T ")"])
    ])

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
  ] mempty

