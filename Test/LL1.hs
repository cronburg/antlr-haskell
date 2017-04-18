module Main where
import Test.Text.ANTLR.Allstar.Grammar
import Text.ANTLR.Allstar.Grammar
import Text.ANTLR.Parser
import Text.ANTLR.AST
import Text.ANTLR.LL1

import Data.Set.Monad (fromList, union, empty, Set(..))
import qualified Data.Set.Monad as Set

import qualified Data.Map.Strict as M

import System.IO.Unsafe (unsafePerformIO)
import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck (Property, quickCheck, (==>))
import qualified Test.QuickCheck.Monadic as TQM

type LL1NonTerminal = String
type LL1Terminal    = String

uPIO = unsafePerformIO

grm :: Grammar () LL1NonTerminal LL1Terminal
grm = dragonBook428

termination = first grm [NT "E"] @?= first grm [NT "E"]

firstF = first grm [NT "F"] @?= fromList [Icon "(", Icon "id"]

noEps = first grm [NT "E"] @?= fromList [Icon "(", Icon "id"]

firstT' =
  first grm [NT "T'"]
  @?=
  fromList [Icon "*", IconEps]

foldEpsTest = foldWhileEpsilon union empty
  [ fromList [Icon "(", Icon "id"]
  , fromList [Icon ")"]
  ]
  @?=
  fromList [Icon "(", Icon "id"]

firstAll =
  ( Set.map ((\nt -> (nt, first grm [nt])) . NT) (ns grm)
    `union`
    Set.map ((\t  -> (t,  first grm [t]))  . T)  (ts grm)
  )
  @?=
  fromList
    [ (NT "E",  fromList [Icon "(", Icon "id"])
    , (NT "E'", fromList [Icon "+", IconEps])
    , (NT "F",  fromList [Icon "(", Icon "id"])
    , (NT "T",  fromList [Icon "(", Icon "id"])
    , (NT "T'", fromList [Icon "*", IconEps])
    , (T "(",   fromList [Icon "("])
    , (T ")",   fromList [Icon ")"])
    , (T "*",   fromList [Icon "*"])
    , (T "+",   fromList [Icon "+"])
    , (T "id",  fromList [Icon "id"])
    ]

grm' :: Grammar () LL1NonTerminal LL1Terminal
grm' = grm

followAll :: IO ()
followAll = let
    fncn :: LL1NonTerminal -> (ProdElem LL1NonTerminal LL1Terminal, Set (Icon LL1Terminal))
    fncn nt = (NT nt, follow grm' nt)
  in Set.map fncn (ns grm')
  @?=
  fromList
    [ (NT "E",  fromList [Icon ")", IconEOF])
    , (NT "E'", fromList [Icon ")", IconEOF])
    , (NT "T",  fromList [Icon ")", Icon "+", IconEOF])
    , (NT "T'", fromList [Icon ")", Icon "+", IconEOF])
    , (NT "F",  fromList [Icon ")", Icon "*", Icon "+", IconEOF])
    ]

parseTableTest =
  parseTable grm
  @?=
  M.fromList (map (\((a,b),c) -> ((a,b), Set.singleton c))
    -- Figure 4.17 of dragon book:
    [ (("E",  Icon "id"), [NT "T", NT "E'"])
    , (("E",  Icon "("),  [NT "T", NT "E'"])
    , (("E'", Icon "+"),  [T "+", NT "T", NT "E'"])
    , (("E'", Icon ")"),  [Eps])
    , (("E'", IconEOF),       [Eps])
    , (("T",  Icon "id"), [NT "F", NT "T'"])
    , (("T",  Icon "("),  [NT "F", NT "T'"])
    , (("T'", Icon "+"),  [Eps])
    , (("T'", Icon "*"),  [T "*", NT "F", NT "T'"])
    , (("T'", Icon ")"),  [Eps])
    , (("T'", IconEOF),       [Eps])
    , (("F",  Icon "id"), [T "id"])
    , (("F",  Icon "("),  [T "(", NT "E", T ")"])
    ])

type LLAST = AST LL1NonTerminal LL1Terminal

action0 EpsE                    = LeafEps
action0 (TermE (Icon t))        = Leaf t
action0 (NonTE (nt, ss, us))    = AST nt ss us

action1 (NonTE (nt, ss, trees)) = uPIO (print ("Act:", nt, ss, trees)) `seq` action0 $ NonTE (nt,ss,trees)
action1 (TermE x) = uPIO (print ("Act:", x)) `seq` action0 $ TermE x
action1 EpsE      = action0 EpsE

dragonPredParse =
  (predictiveParse grm action0 $ map Icon ["id", "+", "id", "*", "id"] ++ [IconEOF])
  @?=
  (Just $ AST "E" [NT "T", NT "E'"]
            [ AST "T" [NT "F", NT "T'"]
                [ AST "F"  [T "id"] [Leaf "id"]
                , AST "T'" [Eps]    [LeafEps]
                ]
            , AST "E'" [T "+", NT "T", NT "E'"]
                [ Leaf "+"
                , AST "T" [NT "F", NT "T'"]
                    [ AST "F" [T "id"] [Leaf "id"]
                    , AST "T'" [T "*", NT "F", NT "T'"]
                        [ Leaf "*"
                        , AST "F" [T "id"] [Leaf "id"]
                        , AST "T'" [Eps] [LeafEps]
                        ]
                    ]
                , AST "E'" [Eps] [LeafEps]
                ]
            ])

singleLang = (defaultGrammar :: Grammar () String Char)
  { s0 = "S"
  , ns = fromList ["S", "X"]
  , ts = fromList ['a']
  , ps =  [ ("S", Prod Pass [NT "X", T 'a'])
          , ("X", Prod Pass [Eps])
          ]
  }

testRemoveEpsilons =
  removeEpsilons singleLang
  @?= singleLang
    { ps =  [ ("S", Prod Pass [NT "X", T 'a'])
            , ("S", Prod Pass [T 'a'])
            ]
    }

singleLang2 = singleLang
  { ts = fromList ['a', 'b']
  , ps =  [ ("S", Prod Pass [NT "X", T 'a', NT "X", T 'b', NT "X"])
          , ("X", Prod Pass [Eps])
          ]
  }

testRemoveEpsilons2 =
  (Set.fromList . ps . removeEpsilons) singleLang2
  @?=
  fromList
    [ ("S", Prod Pass [        T 'a',         T 'b'        ])
    , ("S", Prod Pass [        T 'a',         T 'b', NT "X"])
    , ("S", Prod Pass [        T 'a', NT "X", T 'b'        ])
    , ("S", Prod Pass [        T 'a', NT "X", T 'b', NT "X"])
    , ("S", Prod Pass [NT "X", T 'a',         T 'b'        ])
    , ("S", Prod Pass [NT "X", T 'a',         T 'b', NT "X"])
    , ("S", Prod Pass [NT "X", T 'a', NT "X", T 'b'        ])
    , ("S", Prod Pass [NT "X", T 'a', NT "X", T 'b', NT "X"])
    ]

testRemoveEpsilons3 =
  removeEpsilons dragonBook428
  @?= (defaultGrammar :: Grammar () String String)
    { ns = fromList ["E", "E'", "T", "T'", "F"]
    , ts = fromList ["+", "*", "(", ")", "id"]
    , s0 = "E"
    , ps = [ ("E",  Prod Pass [NT "T", NT "E'"])
           , ("E'", Prod Pass [T "+", NT "T", NT "E'"])
           , ("E'", Prod Pass [Eps]) -- Implicitly epsilon
           , ("T",  Prod Pass [NT "F", NT "T'"])
           , ("T'", Prod Pass [T "*", NT "F", NT "T'"])
           , ("T'", Prod Pass [Eps])
           , ("F",  Prod Pass [T "(", NT "E", T ")"])
           , ("F",  Prod Pass [T "id"])
           ]
    } 

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
  , testCase "testRemoveEpsilons" testRemoveEpsilons
  , testCase "testRemoveEpsilons2" testRemoveEpsilons2
  ] mempty

