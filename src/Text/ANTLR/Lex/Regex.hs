{-# LANGUAGE ScopedTypeVariables #-}
module Text.ANTLR.Lex.Regex where

import Text.ANTLR.Set (Hashable, singleton, fromList)
import Text.ANTLR.Lex.NFA
import qualified Text.ANTLR.Lex.DFA as DFA

data Regex s =
    Epsilon                         -- Regex accepting the empty string
  | Symbol     s                    -- An individual symbol in the alphabet
  | Literal    [s]                  -- A literal sequence of symbols (concatenated together)
  | Class      [s]                  -- A set of alternative symbols (unioned together)
  | Union      (Regex s) (Regex s)  -- Union of two arbitrary regular expressions
  | Concat     [Regex s]            -- Concatenation of 2 or more regular expressions
  | Kleene     (Regex s)            -- Kleene closure of a regex
  | PosClos    (Regex s)            -- Positive closure
  | MultiUnion [Regex s]            -- Union of two or more arbitrary regexs
  | NotClass   [s]                  -- Complement of a character class

regex2nfa' ::
  forall s i. (Hashable i, Ord i, Hashable s, Eq s)
  => (i -> Int) -> (Int -> i) -> Regex s -> NFA s i
regex2nfa' from to r = let
    r2n :: Regex s -> NFA s i
    r2n Epsilon         = list2nfa [ (to 0, (False, singleton   NFAEpsilon), to 1) ]
    r2n (Symbol s)      = list2nfa [ (to 0, (False, singleton $ Edge s),     to 1) ]
    r2n (Union r1 r2)   = nfaUnion   from to (r2n r1) (r2n r2)
    r2n (Concat [])     = r2n Epsilon -- TODO: empty concat 
    r2n (Concat (r:rs)) = foldl (nfaConcat  from to) (r2n r) (map r2n rs)
    r2n (Kleene r1)     = nfaKleene  from to (r2n r1)
    r2n (PosClos r1)    = r2n $ Concat [r1, Kleene r1]
    r2n (Class [])      = r2n Epsilon -- TODO: empty character class shouldn't accept empty string?
    r2n (Class (s:ss))  = list2nfa [ (to 0, (False, fromList $ map Edge $ s:ss), to 1) ] --r2n $ foldl Union (Symbol s) (map Symbol ss)
    r2n (MultiUnion []) = r2n Epsilon
    r2n (MultiUnion (r:rs)) = r2n $ foldl Union r rs
    r2n (Literal ss)    = list2nfa $ map (\(s,i) -> (to i, (False, singleton $ Edge s), to $ i + 1)) (zip ss [0..length ss - 1])
    r2n (NotClass [])     = r2n Epsilon -- TODO
    r2n (NotClass (s:ss)) = list2nfa $ [ (to 0, (True, fromList $ map Edge $ s:ss), to 1) ]
  in r2n r 

regex2nfa :: (Hashable s, Ord s) => Regex s -> NFA s Int
regex2nfa = regex2nfa' id id

regex2dfa :: (Hashable s, Ord s) => Regex s -> DFA.DFA s (DFAState Int)
regex2dfa = nfa2dfa . regex2nfa

