{-# LANGUAGE FlexibleInstances, InstanceSigs #-}
module Text.ANTLR.Allstar.Lex where
import Text.ANTLR.Allstar.Grammar
import Data.Set (fromList, member, (\\), empty)

type TokenName = String
type Token t = (TokenName, t)

tok2term :: Terminal t => Token t -> t
tok2term (n,v) = v

type Lexeme = String
type TerminalName = String

type RegexT  = (TerminalName, Lexeme)
type RegexNT = String

regexGrammar = defaultGrammar
  { ns = fromList ["Regex", "Union", "Concat", "Kleene", "Pos", "Term"]
  , ts = fromList $ map (\a -> (a,"")) ["char", "(", ")", "|", "*"]
  , s0 = "Regex"
  , ps = [ ("Regex", Prod []) ]
  }

