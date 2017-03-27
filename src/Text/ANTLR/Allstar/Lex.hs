module Text.ANTLR.Allstar.Lex where
import Text.ANTLR.Allstar.Grammar
import Data.Set (fromList, member, (\\), empty)

type Token t = (t, String)

termOf :: Terminal t => Token t -> t
termOf = fst

regexGrammar = defaultGrammar
  { ns = fromList ["Regex", "Union", "Concat", "Kleene", "Pos", "Term"]
  , ts = fromList "abcdefghijklmnopqrstuvwxyz"
  , s0 = "Regex"
  , ps = [ ("Regex", Prod []) ]
  }

