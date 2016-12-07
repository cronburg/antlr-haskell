module Main where
import Test.Text.ANTLR.Allstar.Grammar
import Text.ANTLR.Allstar.Grammar
import Text.ANTLR.LL1

import Data.Set (fromList, union, empty)
import qualified Data.Set as S

import System.IO.Unsafe (unsafePerformIO)           
import Data.Monoid                                  
import Test.Framework                               
import Test.Framework.Providers.HUnit               
import Test.Framework.Providers.QuickCheck2         
import Test.HUnit                                   
import Test.QuickCheck (Property, quickCheck, (==>))
import qualified Test.QuickCheck.Monadic as TQM     

grm = dragonBook428

termination = first grm (NT "E") @?= first grm (NT "E")

firstF = first grm (NT "F") @?= fromList [Term "(", Term "id"]

noEps = first grm (NT "E") @?= fromList [Term "(", Term "id"]

firstT' =
  first grm (NT "T'")
  @?=
  fromList [Term "*", Eps']

foldEpsTest = foldWhileEpsilon union empty
  [ fromList [Term "(", Term "id"]
  , fromList [Term ")"]
  ]
  @?=
  fromList [Term "(", Term "id"]

firstAll =
  ( S.map ((\nt -> (nt, first grm nt)) . NT) (ns grm)
    `union`
    S.map ((\t  -> (t,  first grm t))  . T)  (ts grm)
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
                                                                                         

main :: IO ()
main = defaultMainWithOpts
  [ testCase "fold_epsilon" foldEpsTest
  , testCase "termination" termination
  , testCase "no_epsilon" noEps
  , testCase "firstF" firstF
  , testCase "firstT'" firstT'
  , testCase "firstAll" firstAll
  ] mempty

