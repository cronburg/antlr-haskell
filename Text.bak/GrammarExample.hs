dumbGrammar :: Grammar
dumbGrammar = Grammar { gN = ["S", "A", "B", "I", "D"]
                      , gT = ["1","2","3","+","-","*"]
                      , gS = "S" 
                      , gP = [ ContextFree "S" [Left "A"]
                             , ContextFree "S" [Left "B"]
                             , ContextFree "S" [Left "D"]
                             , ContextFree "A" [Left "I", Right "+", Left "I"]
                             , ContextFree "B" [Left "I", Right "-", Left "I"]
                             , ContextFree "I" [Right "1"]
                             , ContextFree "I" [Right "2"]
                             , ContextFree "I" [Right "3"]
                             , Predicated "D" (Just $ \_ -> True) [Left "I", Right "*", Left "I"]                
                             ]   
                      , gPi = [(\_ -> True)]
                      , gM  = []}

