{-# LANGUAGE QuasiQuotes, TemplateHaskell, ScopedTypeVariables #-}
module Main where
-- Project imports go here, e.g.:
--import Language.Chisel.Tokenizer
import Text.ANTLR.Lex.Tokenizer (Token(..))
import Text.ANTLR.Parser (AST(..))
import Language.Chisel.Grammar
import Language.Chisel.Syntax
import Text.ANTLR.Grammar (Grammar(..), ProdElem(..))
import Language.ANTLR4.FileOpener (open)

import System.IO.Unsafe (unsafePerformIO)
import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit hiding ((@?=), assertEqual)
import Test.QuickCheck (Property, quickCheck, (==>))
import qualified Test.QuickCheck.Monadic as TQM

import Test.Text.ANTLR.HUnit
import Debug.Trace as D
import qualified Text.ANTLR.LR as LR
import Text.ANTLR.Pretty (pshow)
import qualified Data.Text as T

chi = id

ghc_val = [open| Test/Language/Chisel/Examples/GHC.chi |]
tokenizeGHC_val = tokenize ghc_val

tokenizeGHC_exp =
  [ upperID "Heap", ws " ", lowerID "m", ws " ", lowerID "k", ws " ", arrow
  , ws "\n  ", pound, ws " ", upperID "MegaBlock", ws " "
  , vertbar, int 2, carrot, lowerID "m", ws " ", prim Byte, vertbar, ws " "
  , atsymbol, lparen, int 2, carrot, lowerID "m", ws " ", prim Byte, rparen
  , ws " ", arrow, ws "\n    ", linecomm "// -------Megablock \"Header\"------------------"
  , ws "\n    ", lparen, ws " ", upperID "Descrs", ws " "
  , vertbar, int 2, carrot, lowerID "k", ws " ", prim Byte, vertbar, ws " "
  , arrow, ws "\n      ", lparen, ws " ", lowerID "padMB", ws " ", prim Byte
  , ws "\n      ", linecomm "// +++++++++++++++++++++++++++++++++++++++++\n"
  , ws "      ", comma, ws " ", lowerID "bds", ws " "
  , colon, ws " ", lowerID "n", ws " ", upperID "BlockDescr", ws " "
  , vertbar, int 2, carrot, lowerID "d", ws " ", prim Byte, vertbar
  , ws " ", atsymbol, lparen, int 2, carrot, lowerID "k", ws " ", prim Byte, rparen
  , ws " ", arrow, ws "\n        "
  , lparen, ws " ", lowerID "start", ws " ", colon, ws " "
  , upperID "Ptr", ws " ", upperID "Stg", dot, upperID "Word", ws "\n        "
  , comma, ws " ", lowerID "free", ws "  ", colon, ws " "
  , upperID "Ptr", ws " ", upperID "Stg", dot, upperID "Word", ws "\n        "
  , comma, ws " ", lowerID "link", ws "  ", colon, ws " "
  , upperID "Ptr", ws " ", upperID "BlockDescr", ws "\n        "
  , comma, ws " ", lparen, ws " ", lowerID "back", ws "   ", colon, ws " "
  , upperID "Ptr", ws " ", upperID "BlockDescr", ws "\n          "
  , vertbar, ws " ", lowerID "bitmap", ws " ", colon, ws " "
  , upperID "Ptr", ws " ", upperID "Stg", dot, upperID "Word", ws "\n          "
  , vertbar, ws " ", lowerID "scan", ws "   ", colon, ws " "
  , upperID "Ptr", ws " ", upperID "Stg", dot, upperID "Word", rparen, ws "\n        "
  , comma, ws " ", lowerID "gen", ws "     ", colon, ws " "
  , upperID "Ptr", ws " ", upperID "Generation", ws "\n    ", comma, ws " "
  , lowerID "gen_no", ws "  ", colon, ws " "
  , upperID "Stg", dot, upperID "Word16", ws "\n      "
  , comma, ws " ", lowerID "dest_no", ws " ", colon, ws " "
  , upperID "Stg", dot, upperID "Word16", ws "\n        "
  , comma, ws " ", lowerID "node", ws "    ", colon, ws " "
  , upperID "Stg", dot, upperID "Word16", ws "\n        "
  , comma, ws " ", upperID "Flags", ws " "
  , vertbar, upperID "Stg", dot, upperID "Word16", vertbar, ws " ", arrow, ws "\n          "
  , lparen, ws " ", upperID "LARGE", ws "  ", vertbar, ws " "
  , upperID "EVACUATED", ws "", vertbar, ws " "
  , upperID "FREE", ws "\n          ", vertbar, ws " "
  , upperID "PINNED", ws " ", vertbar, ws " "
  , upperID "MARKED", ws "     ", vertbar, ws " "
  , upperID "KNOWN", ws "\n          ", vertbar, ws " "
  , upperID "EXEC", ws "   ", vertbar, ws " "
  , upperID "FRAGMENTED", ws " ", vertbar, ws " "
  , upperID "SWEPT", ws "\n          ", vertbar, ws " "
  , upperID "COMPACT", ws " ", rparen, ws "\n        ", comma, ws " "
  , lowerID "n_blocks", ws " ", colon, ws " "
  , upperID "Stg", dot, upperID "Word32", ws "\n        "
  , comma, ws " ", lowerID "padD", ws " ", prim Byte, rparen, rparen, ws "\n      "
  , linecomm "// +++++++++++++++++++++++++++++++++++++++++\n", ws "    "
  , linecomm "// -------Megablock payload-------------------\n", ws "    ", comma, ws " "
  , lowerID "blocks", ws " ", colon, ws " ", lowerID "n", ws " "
  , upperID "Block", ws " ", vertbar, int 2, carrot
  , lowerID "k", ws " ", prim Byte, vertbar, ws " "
  , atsymbol, lparen, int 2, carrot, lowerID "k", ws " ", prim Byte, rparen
  , ws " ", arrow, ws "\n      ", lparen, ws " "
  , lowerID "closures", ws " ", colon, ws " ", pound, ws " "
  , upperID "Stg", dot, upperID "Closures", ws "\n      ", comma, ws " "
  , lowerID "free", ws "     ", colon, ws " ", pound, ws " ", prim Byte, rparen
  , ws "\n    ", rparen, ws "\n", EOF
  ]

tokenizeGHC =
  tokenizeGHC_val
  @?=
  tokenizeGHC_exp

tokenizeGHC2 =
  dropWhile (\(a,b) -> a == b) (zip tokenizeGHC_val tokenizeGHC_exp)
  @?=
  []

tokenizeSmall = tokenize "Foo x -> x Bar"

parseTestSmall =
  D.traceShowId (parse "Foo x -> x Bar")
  @?=
  LR.ResultAccept
    ( AST NT_chiselProd [NT NT_prodSimple]
      [ AST NT_prodSimple [NT NT_prodID, NT NT_formals, T T_2, NT NT_group]
        [ AST NT_prodID [T T_UpperID] [Leaf $ upperID "Foo"]
        , AST NT_formals [T T_LowerID] 
          [ Leaf $ lowerID "x"]
        , Leaf arrow
        , AST NT_group [NT NT_groupExp1]
            [ AST NT_groupExp1 [NT NT_arith, NT NT_prodApp]
              [ AST NT_arith [T T_LowerID] [Leaf $ lowerID "x"]
              , AST NT_prodApp [NT NT_prodID]
                [ AST NT_prodID [T T_UpperID] [Leaf $ upperID "Bar"] ]
                ]]]])

tokenizeSmallTest =
  tokenizeSmall
  @?=
  [upperID "Foo", ws " ", lowerID "x", ws " ", arrow, ws " ", lowerID "x", ws " ", upperID "Bar", EOF]

parseGHCTestBig =
  case parse ghc_val of
    (LR.ResultAccept _)   -> (1 :: Int) @?= 1
    e                     -> e @?= LR.ResultAccept LeafEps

testPrettify =
  unsafePerformIO (putStr $ T.unpack $ pshow (chiselGrammar :: Grammar () ChiselNTSymbol ChiselTSymbol))
  @?= ()

main :: IO ()
main = defaultMainWithOpts
  [ testCase "Tokenize GHC" tokenizeGHC
  , testCase "Tokenize GHC2" tokenizeGHC2
  , testCase "Tokenize Small" tokenizeSmallTest
  , testCase "Parse Test (Small)" parseTestSmall
  , testCase "Parse GHC"  parseGHCTestBig
  , testCase "Prettify speed" testPrettify
  ] mempty

