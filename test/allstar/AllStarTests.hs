{-# LANGUAGE TypeFamilies #-}

module AllStarTests where

import Test.HUnit
import Text.ANTLR.Allstar.ParserGenerator
import qualified Data.Set as DS

--------------------------------TESTING-----------------------------------------

instance Token Char where
  type Label Char = Char
  type Literal Char = Char
  getLabel c = c
  getLiteral c = c

instance Token (a, b) where
  type Label (a, b) = a
  type Literal (a, b) = b
  getLabel (a, b) = a
  getLiteral (a, b) = b

atnEnv = DS.fromList [ -- First path through the 'S' ATN
                       (Init 'S', GS EPS, Middle 'S' 0 0),
                       (Middle 'S' 0 0, GS (NT 'A'), Middle 'S' 0 1),
                       (Middle 'S' 0 1, GS (T 'c'), Middle 'S' 0 2),
                       (Middle 'S' 0 2, GS EPS, Final 'S'),

                       -- Second path through the 'S' ATN
                       (Init 'S', GS EPS, Middle 'S' 1 0),
                       (Middle 'S' 1 0, GS (NT 'A'), Middle 'S' 1 1),
                       (Middle 'S' 1 1, GS (T 'd'), Middle 'S' 1 2),
                       (Middle 'S' 1 2, GS EPS, Final 'S'),

                       -- First path through the 'A' ATN
                       (Init 'A', GS EPS, Middle 'A' 0 0),
                       (Middle 'A' 0 0, GS (T 'a'), Middle 'A' 0 1),
                       (Middle 'A' 0 1, GS (NT 'A'), Middle 'A' 0 2),
                       (Middle 'A' 0 2, GS EPS, Final 'A'),

                       -- Second path through the 'A' ATN
                       (Init 'A', GS EPS, Middle 'A' 1 0),
                       (Middle 'A' 1 0, GS (T 'b'), Middle 'A' 1 1),
                       (Middle 'A' 1 1, GS EPS, Final 'A')]


-- For now, I'm only checking whether the input was accepted--not checking the derivation.

-- Example from the manual trace of ALL(*)'s execution
parseTest1 = TestCase (assertEqual "for parse [a, b, c],"
                                   (Right (Node 'S'
                                            [Node 'A'
                                              [Leaf 'a',
                                               Node 'A'
                                                [Leaf 'b']],
                                             Leaf 'c']))
                                   (parse ['a', 'b', 'c'] (NT 'S') atnEnv True))
                                   
-- Example #1 from the ALL(*) paper
parseTest2 = TestCase (assertEqual "for parse [b, c],"
                                    (Right (Node 'S'
                                             [Node 'A'
                                               [Leaf 'b'],
                                              Leaf 'c']))
                                    (parse ['b', 'c'] (NT 'S') atnEnv True))
                                    
-- Example #2 from the ALL(*) paper
parseTest3 = TestCase (assertEqual "for parse [b, d],"
                                   (Right (Node 'S'
                                            [Node 'A'
                                              [Leaf 'b'],
                                             Leaf 'd']))
                                   (parse ['b', 'd'] (NT 'S') atnEnv True))
                                    
-- Input that requires more recursive traversals of the A ATN
parseTest4 = TestCase (assertEqual "for parse [a a a b c],"
                                   (Right (Node 'S'
                                            [Node 'A'
                                              [Leaf 'a',
                                               Node 'A'
                                                [Leaf 'a',
                                                 Node 'A'
                                                  [Leaf 'a',
                                                   Node 'A'
                                                    [Leaf 'b']]]],
                                             Leaf 'c']))
                                   (parse ['a', 'a', 'a', 'b', 'c'] (NT 'S') atnEnv True))

-- Make sure that the result of parsing an out-of-language string has a Left tag.             
parseTest5 = TestCase (assertEqual "for parse [a b a c],"
                                   True
                                   (let parseResult = parse ['a', 'b', 'a', 'c'] (NT 'S') atnEnv True
                                        isLeft pr = case pr of
                                                      Left _ -> True
                                                      _ -> False
                                    in  isLeft parseResult))

-- To do: Update these tests so that they use the new ATN state representation.
{-

conflictsTest = TestCase (assertEqual "for getConflictSetsPerLoc()"
                         
                                      ([[(MIDDLE 5, 1, []), (MIDDLE 5, 2, []),(MIDDLE 5, 3, [])],
                                        [(MIDDLE 5, 1, [MIDDLE 1]), (MIDDLE 5, 2, [MIDDLE 1])],
                                        [(MIDDLE 7, 2, [MIDDLE 6, MIDDLE 1])]] :: [[ATNConfig Char]])
                                         
                                      (getConflictSetsPerLoc (D [(MIDDLE 5, 1, []),
                                                                 (MIDDLE 5, 2, []),
                                                                 (MIDDLE 5, 3, []),
                                                                 (MIDDLE 5, 1, [MIDDLE 1]),
                                                                 (MIDDLE 5, 2, [MIDDLE 1]),
                                                                 (MIDDLE 7, 2, [MIDDLE 6, MIDDLE 1])])))

prodsTest = TestCase (assertEqual "for getProdSetsPerState()"
                     
                                  ([[(MIDDLE 5, 1, []),
                                     (MIDDLE 5, 2, []),
                                     (MIDDLE 5, 3, []),
                                     (MIDDLE 5, 1, [MIDDLE 1]),
                                     (MIDDLE 5, 2, [MIDDLE 1])],
                                     
                                    [(MIDDLE 7, 2, [MIDDLE 6, MIDDLE 1])]] :: [[ATNConfig Char]])
                                         
                                  (getProdSetsPerState (D [(MIDDLE 5, 1, []),
                                                           (MIDDLE 5, 2, []),
                                                           (MIDDLE 5, 3, []),
                                                           (MIDDLE 5, 1, [MIDDLE 1]),
                                                           (MIDDLE 5, 2, [MIDDLE 1]),
                                                           (MIDDLE 7, 2, [MIDDLE 6, MIDDLE 1])])))

-}


ambigATNEnv = DS.fromList [(Init 'S', GS EPS, Middle 'S' 0 0),
                           (Middle 'S' 0 0, GS (T 'a'), Middle 'S' 0 1),
                           (Middle 'S' 0 1, GS EPS, Final 'S'),
               
                           (Init 'S', GS EPS, Middle 'S' 1 0),
                           (Middle 'S' 1 0, GS (T 'a'), Middle 'S' 1 1),
                           (Middle 'S' 1 1, GS EPS, Final 'S'),
               
                           (Init 'S', GS EPS, Middle 'S' 2 0),
                           (Middle 'S' 2 0, GS (T 'a'), Middle 'S' 2 1),
                           (Middle 'S' 2 1, GS (T 'b'), Middle 'S' 2 2),
                           (Middle 'S' 2 2, GS EPS, Final 'S')]

ambigParseTest1 = TestCase (assertEqual "for parse [a],"
                                        True
                                        (let parseResult = parse ['a'] (NT 'S') ambigATNEnv True
                                             isLeft pr = case pr of
                                                           Left _ -> True
                                                           _ -> False
                                         in  isLeft parseResult))

ambigParseTest2 = TestCase (assertEqual "for parse [a b],"
                                        (Right (Node 'S'
                                                 [Leaf 'a',
                                                  Leaf 'b']))
                                        (parse ['a', 'b'] (NT 'S') ambigATNEnv True))

        
tests = [TestLabel "parseTest1"    parseTest1,
         TestLabel "parseTest2"    parseTest2,
         TestLabel "parseTest3"    parseTest3,
         TestLabel "parseTest4"    parseTest4,
         TestLabel "parseTest5"    parseTest5,
                  
         --TestLabel "conflictsTest" conflictsTest,
         --TestLabel "prodsTest"     prodsTest,

         TestLabel "ambigParseTest1" ambigParseTest1,
         TestLabel "ambigParseTest2" ambigParseTest2]
       
main = runTestTT (TestList tests)
