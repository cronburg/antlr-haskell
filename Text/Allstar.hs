module Text.AllStar where

ATN
State
-- dictionary of nonterminals to corresponding dfa
type dfa = [(NonTerminal, DFA)]
input

-- 1) Parse 
-- INPUT: start symbol S in set of non-terminals
-- OUTPUT: ??? !!!! TARA FIGURE OUT !!!
parse :: State -> ()
-- Define the state and atn which should be available to everything
parse S = 
    let gamma0     = empty_stack
        -- adaptivePredict
        i0         = adaptivePredict S gamma
        -- getBranch in ATN returns a prodElem
        p0         = getBranch S i
        -- if we've reached the final state for start state; return void
        parse_loop gamma i p = 
            case p of (Final nt) -> if nt = (getName S) then () 
                                        else stackPop gamma 
                                      --returns (q, g') for p, gamma
                      (Branch nt transition) gamma  -> 
                         case transition of
                            (ContextFree [])         -> 
                                parse_loop (Final nt) gamma
                            (ContextFree (T b):qs)   -> 
                                if b = input 
                        --We need to figure out how we are getting user
                        --input here. get next input and set to current here
                                    then parse_loop gamma i (Branch nt xs)
                                        --this is a parsing error. do something.
                                    else parse error
                            (ContextFree (NT B):qs)  ->
                                --push puts qs:gamma
                                let gamma' = push qs gamma
                                    i'     = adaptivePredict B gamma'
                                    p'     = getBranch atn B i'
                                in  parse_loop gamma' i' p'
                            (Mutated m)              ->
                                --use a monad to modify state
                                --(mutate state m)
                                parse_loop gamma i (Final nt)
                            (Predicated (Just pi) q) ->
                                --(check pi in state) returns bool
                                if (check pi state) 
                                    then (parse_loop gamma i (ContextFree q))
                                    --fix parse error
                                    else parse error
                            (Epsilon)                ->
                                parse_loop gamma i (Final nt)
    in parse_loop gamma0 i0 p0

-- 2) adaptivePredict
-- !!! DEFINE PARSER STACK !!!
adaptivePredict :: (State, ParseStack) -> Int
-- something about input index here. again input problem. 
adaptivePredict (state, gamma) =
    let start = index input
        if (existsBranches state (\x -> case x of (Predicated _ _) -> true
                                                        _   -> false))
            then LLpredict(state, start, gamma)
                --undo stream position changes input.seek(start)
            else if (lookup (getName state) dfa) = Nothing
    -- # means no stack info. make sure you can use this symbol or some other.
    -- used as argument for gamma call stack
                    then let D_0 = startState state #
                             F_dfa = foldriBranch (\x a i -> (Final i):a) 
                                                                [] state
                            --what is D_error??
                             Q_dfa = D_0 ++ F_dfa ++ D_error 
                        -- need to get terminals T form grammar, maybe ATN?
                         in ((getName state), (DFA {dfaQ = Q_dfa, dfaSigma = T,
                            dfaDelta = [], dfaD_0 = D_0, dfaF = F_dfa})):dfa
                        --dfa is the top-level dfa accessible to all.

-- 3) startState
-- DEFINE DFA STATE/DFA
startState :: (State, ParseStack) -> DFAState
startState (state, gamma) = 
        --DFA must be a mutable structure
        foldriBranch (\x d i -> case x of (Predicated pi alpha) ->
                                        if (eval pi)
-- MAKE DFA MODULE 
                                            then (add d (closure 
                                                ({}, d, (state, i, gamma))))
                                            else d
                                            _                   ->
                                        (add d (closure ({}, d, (???, i, 
                                                gamma))))) (new DFA) 
            -- accumulator is the DFA being returned and built
        --write eval function for pi. Or is it Maybe - Just?
 
-- 4) SLLpredict
-- 5) target
-- 6) LLpredict
-- 7) closure
-- 8) getConflictSetsPerLoc
-- 9) getProdSetsPerState
