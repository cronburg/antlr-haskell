module Main where
import Test.Text.ANTLR.Allstar.Grammar
import Text.ANTLR.Allstar.Grammar
import Text.ANTLR.LL1

import Data.Set (fromList, union, empty, Set(..))
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

iS = InputSymbol

type LL1NonTerminal = String
type LL1Terminal    = String

uPIO = unsafePerformIO

grm :: Grammar () LL1NonTerminal LL1Terminal
grm = dragonBook428

termination = first grm [NT "E"] @?= first grm [NT "E"]

firstF = first grm [NT "F"] @?= fromList [iS "(", iS "id"]

noEps = first grm [NT "E"] @?= fromList [iS "(", iS "id"]

firstT' =
  first grm [NT "T'"]
  @?=
  fromList [iS "*", Eps']

foldEpsTest = foldWhileEpsilon union empty
  [ fromList [iS "(", iS "id"]
  , fromList [iS ")"]
  ]
  @?=
  fromList [iS "(", iS "id"]

firstAll =
  ( S.map ((\nt -> (nt, first grm [nt])) . NT) (ns grm)
    `union`
    S.map ((\t  -> (t,  first grm [t]))  . T)  (ts grm)
  )
  @?=
  fromList
    [ (NT "E",  fromList [iS "(", iS "id"])
    , (NT "E'", fromList [iS "+", Eps'])
    , (NT "F",  fromList [iS "(", iS "id"])
    , (NT "T",  fromList [iS "(", iS "id"])
    , (NT "T'", fromList [iS "*", Eps'])
    , (T "(",   fromList [iS "("])
    , (T ")",   fromList [iS ")"])
    , (T "*",   fromList [iS "*"])
    , (T "+",   fromList [iS "+"])
    , (T "id",  fromList [iS "id"])
    ]

grm' :: Grammar () LL1NonTerminal LL1Terminal
grm' = grm

followAll :: IO ()
followAll = let
    fncn :: LL1NonTerminal -> (ProdElem LL1NonTerminal LL1Terminal, Set (InputSymbol LL1Terminal))
    fncn nt = (NT nt, follow grm' nt)
  in S.map fncn (ns grm')
  @?=
  fromList
    [ (NT "E",  fromList [iS ")", EOF])
    , (NT "E'", fromList [iS ")", EOF])
    , (NT "T",  fromList [iS ")", iS "+", EOF])
    , (NT "T'", fromList [iS ")", iS "+", EOF])
    , (NT "F",  fromList [iS ")", iS "*", iS "+", EOF])
    ]

parseTableTest =
  parseTable grm
  @?=
  M.fromList (map (\((a,b),c) -> ((a,b), S.singleton c))
    -- Figure 4.17 of dragon book:
    [ (("E",  iS "id"), [NT "T", NT "E'"])
    , (("E",  iS "("),  [NT "T", NT "E'"])
    , (("E'", iS "+"),  [T "+", NT "T", NT "E'"])
    , (("E'", iS ")"),  [Eps])
    , (("E'", EOF),       [Eps])
    , (("T",  iS "id"), [NT "F", NT "T'"])
    , (("T",  iS "("),  [NT "F", NT "T'"])
    , (("T'", iS "+"),  [Eps])
    , (("T'", iS "*"),  [T "*", NT "F", NT "T'"])
    , (("T'", iS ")"),  [Eps])
    , (("T'", EOF),       [Eps])
    , (("F",  iS "id"), [T "id"])
    , (("F",  iS "("),  [T "(", NT "E", T ")"])
    ])

data UAST =
    ULeafEps
  | ULeaf LL1Terminal
  | UAST  LL1NonTerminal
          (Symbols LL1NonTerminal LL1Terminal)
          [UAST]
  deriving (Eq, Ord, Show)

action0 EpsE                    = ULeafEps
action0 (TermE t)               = ULeaf t
action0 (NonTE (nt, ss, us))    = UAST nt ss us

action1 (NonTE (nt, ss, trees)) = uPIO (print ("Act:", nt, ss, trees)) `seq` action0 $ NonTE (nt,ss,trees)
action1 (TermE x) = uPIO (print ("Act:", x)) `seq` action0 $ TermE x
action1 EpsE      = action0 EpsE

dragonPredParse =
  (predictiveParse grm action0 $ map iS ["id", "+", "id", "*", "id"] ++ [EOF])
  @?=
  (Just $ UAST "E" [NT "T", NT "E'"]
            [ UAST "T" [NT "F", NT "T'"]
                [ UAST "F"  [T "id"] [ULeaf "id"]
                , UAST "T'" [Eps]    [ULeafEps]
                ]
            , UAST "E'" [T "+", NT "T", NT "E'"]
                [ ULeaf "+"
                , UAST "T" [NT "F", NT "T'"]
                    [ UAST "F" [T "id"] [ULeaf "id"]
                    , UAST "T'" [T "*", NT "F", NT "T'"]
                        [ ULeaf "*"
                        , UAST "F" [T "id"] [ULeaf "id"]
                        , UAST "T'" [Eps] [ULeafEps]
                        ]
                    ]
                , UAST "E'" [Eps] [ULeafEps]
                ]
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
  , testCase "dragonIsValid" $ validGrammar grm @?= True
  , testCase "dragonIsLL1" $ isLL1 grm @?= True
  , testCase "dragonParseTable" parseTableTest
  , testCase "dragonPredParse" dragonPredParse
  ] mempty

