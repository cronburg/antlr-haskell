module Text.DFA where

-- Placeholder record. Fix the member types. 

data dfaState = Start
              | Branch
              | Final Int

data DFA = DFA { dfaQ       :: [State]
                 dfaSigma   :: [Edge]
                 dfaDelta   :: [Edge]
                 dfaD_0     :: [State]
                 dfaF       :: [State]
                }
