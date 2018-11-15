import Text.ParserCombinators.Parsec as Parsec
import Data.Char
import ParserGenerator.AllStar

--Functions for parsing a context-free grammar in BNF form

grammarFile = endBy production eol

production =
    do l <- nonterminal
       skipMany spacesAndTabs
       string "::="
       skipMany spacesAndTabs
       r <- rightSide
       return (l, r) 

rightSide = sepBy expansion expansionSep

expansion = many symbol

expansionSep =
    do skipMany spacesAndTabs
       char '|'
       skipMany spacesAndTabs

symbol = nonterminal <|> terminal

nonterminal = upper

terminal    = lower

spacesAndTabs = oneOf " \t"

eol = char '\n'


parseGrammar :: String -> Either ParseError [(Char, [String])]
parseGrammar input = Parsec.parse grammarFile "(unknown)" input


--Functions for converting a parsed grammar to an ATN environment

grammarToATNEnv g =
  case parseGrammar g of
    Left e            -> error "could not parse grammar"
    Right productions ->
      let (nonterminals, _) = unzip productions
      in  zip nonterminals (map productionToATN productions)


productionToATN (nt, expansions) =
  map (\(n, exp) -> expansionToATNPath nt exp n) (zip [1..] expansions)

expansionToATNPath nt exp choiceNum =
  let buildMiddleAndFinalStates n symbols =
        case symbols of
          []           -> [(MIDDLE n, EPS, FINAL nt)]
          s : symbols' ->
            let edgeLabel = if isUpper s then NT s else T s
            in  (MIDDLE n, edgeLabel, MIDDLE (n + 1)) : buildMiddleAndFinalStates (n + 1) symbols'
  in  (INIT nt, EPS, CHOICE nt choiceNum)  :
      (CHOICE nt choiceNum, EPS, MIDDLE 1) :
      buildMiddleAndFinalStates 1 exp

main =
    do g <- getContents
       putStrLn (show (grammarToATNEnv g))
