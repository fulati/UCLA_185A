module FinalProject01 where

import Control.Applicative(liftA, liftA2, liftA3)
import Data.List

import CFGParsing

bottomUp :: (Eq nt, Eq t) => CFG nt t -> [t] -> [[ParseStep nt t]]
bottomUp cfg input =
  let (nts, ts, start, rules) = cfg in
  let startingConfig = ([], input) in
  let goalConfig = ([NoBar start], []) in
  parser [shift, reduce] rules startingConfig goalConfig
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- These functions are placeholders to work with 'bottomUp' in Part 1.3. 
-- You should replace 'undefined' in these functions with your own code.

--1.1
--A: Check if a rewrite rule is in Chomsky Normal Form
isRuleCNF :: RewriteRule nt t -> Bool
isRuleCNF rule = 
    case rule of
        NTRule nt [_,_] -> True
        TRule nt t -> True
        NoRule -> True
        _ -> False

--B: Check if CFG is in Chomsky Normal Form
isCNF :: CFG nt t -> Bool
isCNF cfg = 
    let (nts, ts, initial, r) = cfg in
    all isRuleCNF r  -- Validates all rules are in CFG

--1.2: Finds all paths to goal state in a FSA
pathsToGoalFSA :: (Eq st, Eq sy) => ((st,[sy]), [(st,[sy])]) -> [(st,sy,st)] ->[(st,[sy])] -> [[(st,[sy])]]
pathsToGoalFSA (current, history) rules goals =
    case elem current goals of
        True -> [history ++ [current]]  -- Reached the goal state
        False -> concatMap (\x -> pathsToGoalFSA (x, history ++ [current]) rules goals) (consumeFSA rules current)  -- Recursively explore next states



-------------------------------------------------------------------------------
--                            Parser Function
-------------------------------------------------------------------------------
-- Parser function that applies the transition steps to parse an input from start to goal configs
parser :: (Eq nt, Eq t)
       => [[RewriteRule nt t] -> Config nt t -> [ParseStep nt t]]
          -- ^ List of transition steps. ^
       -> [RewriteRule nt t]  -- Rules from the CFG.
       -> Config nt t         -- Starting configuration.
       -> Config nt t         -- Goal configuration.
       -> [[ParseStep nt t]]  -- List of possible parses.
parser transitionSteps rules startingConfig goalConfig = initialRowParser True transitionSteps rules startingConfig goalConfig  -- Call to the initialRowParser with flag set to True


-- Parser Helper function that recursively parses and makes sure to add the very first row ("NoTransition")
initialRowParser :: (Eq nt, Eq t)
             => Bool
             -> [[RewriteRule nt t] -> Config nt t -> [ParseStep nt t]]
             -> [RewriteRule nt t]
             -> Config nt t
             -> Config nt t
             -> [[ParseStep nt t]]
initialRowParser isInitial transitionSteps rules startingConfig goalConfig =
    case startingConfig == goalConfig of 
        True -> [[]]
        False -> 
            let initialStep = if isInitial then [ParseStep NoTransition NoRule startingConfig] else []  -- Add the initial "NoTransition" step
            in map (\steps -> initialStep ++ steps) -- Combine the initial step with the other step from the recursive parsing
                    (concatMap (\transStep ->  -- Go through all transitions
                            concatMap (\step ->  -- For each transition, go through the generated parse steps
                                    map (\steps -> step : steps)  -- For each parse step, recursively call parser
                                        (initialRowParser False transitionSteps rules (getConfig step) goalConfig))  -- Recursively call the initialRowParser with flag set to False
                            (transStep rules startingConfig))
                    transitionSteps)



-------------------------------------------------------------------------------
--                            Bottom-Up Parsing
-------------------------------------------------------------------------------
-- Moves the input to the stack if there is a matching TRule  
shift :: (Eq nt, Eq t) => [RewriteRule nt t] -> Config nt t -> [ParseStep nt t]
shift rules (stack, input) =
    case input of 
        [] -> []
        (x : rest) -> 
            map (\rule -> ParseStep Shift rule (stack ++ [NoBar (lhs rule)], rest))  -- Add the matched symbol to the stack and update the input
                (filter (\rule -> case rule of  -- Filter the rules to find matching TRule
                                    TRule _ t -> t == x  -- Match the TRule
                                    _ -> False) rules)   -- Ignore rules that aren't TRule

-- Combines symbols on the stack into a single symbol based on the rule
reduce :: (Eq nt, Eq t) => [RewriteRule nt t] -> Config nt t -> [ParseStep nt t]
reduce rules (stack, input) =
    concatMap (\rule -> case rule of
        NTRule lhs rhs ->  -- Only process NTRule
            let prefix = reverse (take (length rhs) (reverse stack)) in  -- Extract the top of the stack corresponding to the rhs of the rule
            let restStack = reverse (drop (length rhs) (reverse stack)) in  -- Extract rest of the stack 
            case prefix == map NoBar rhs of  -- Check if top of stack matches rhs
                True  -> [ParseStep Reduce rule (restStack ++ [NoBar lhs], input)]  -- If match, then create ParseStep and replace rhs with lhs on the stack
                False -> []
        _ -> []) rules



-------------------------------------------------------------------------------
--                            Top-Down Parsing
-------------------------------------------------------------------------------
-- Removes the input from the stack if there is a matching TRule
match :: (Eq nt, Eq t) => [RewriteRule nt t] -> Config nt t -> [ParseStep nt t]
match rules (stack, input) =
    case stack of
        NoBar nt : restStack ->  -- Check if top is NoBar nt
            case input of
                x : restInput ->
                    map (\rule -> ParseStep Match rule (restStack, restInput))  -- Create ParseStep for each matching TRule
                        (filter (\rule -> case rule of  -- Filter only matching TRules
                                            TRule nt' t' -> nt' == nt && t' == x  
                                            _ -> False) rules)
                _ -> []
        _ -> []

-- Adds symbols onto the stack by predicting what symbol follows based on grammar rule
predict :: (Eq nt, Eq t) => [RewriteRule nt t] -> Config nt t -> [ParseStep nt t]
predict rules (stack, input) =
    case stack of
        NoBar nt : restStack ->
            map (\rule -> ParseStep Predict rule (map NoBar (rhsNTRule rule) ++ restStack, input))  -- Add rhs of NTRule to the stack
                (filter (\rule -> case rule of
                                    NTRule lhs _ -> lhs == nt  -- Match if NTRule's lhs == nt
                                    _ -> False) rules)
        _ -> []

-- Implements topDown parsing
topDown :: (Eq nt, Eq t) => CFG nt t -> [t] -> [[ParseStep nt t]]
topDown cfg input = 
    let (nts, ts, start, rules) = cfg in
    let startingConfig = ([NoBar start], input) in
    let goalConfig = ([], []) in
    parser [predict, match] rules startingConfig goalConfig



-------------------------------------------------------------------------------
--                            Left-Corner Parsing
-------------------------------------------------------------------------------
-- Moves the input to the stack if there is a matching TRule  
shiftLC :: (Eq nt, Eq t) => [RewriteRule nt t] -> Config nt t -> [ParseStep nt t]
shiftLC rules (stack, input) =
    case input of 
        [] -> []
        (x : rest) -> 
            map (\rule -> ParseStep Shift rule ([NoBar (lhs rule)] ++ stack, rest)) 
                (filter (\rule -> case rule of 
                                    TRule _ t -> t == x
                                    _ -> False) rules)

-- Removes the input from the stack if there is a matching TRule, now includes Bar symbols
matchLC :: (Eq nt, Eq t) => [RewriteRule nt t] -> Config nt t -> [ParseStep nt t]
matchLC rules (stack, input) =
    case stack of
        Bar nt : restStack ->
            case input of
                x : restInput ->
                    map (\rule -> ParseStep Match rule (restStack, restInput)) -- Create ParseStep for each matching TRule
                        (filter (\rule -> case rule of -- Filter only matching TRules
                                            TRule nt' t' -> nt' == nt && t' == x; 
                                            _ -> False) rules)
                _ -> []
        _ -> []

-- Expands a partially completed structure by predicting what symbol follows based on grammar rule
predictLC :: (Eq nt, Eq t) => [RewriteRule nt t] -> Config nt t -> [ParseStep nt t]
predictLC rules (stack, input) =
    case stack of
        NoBar nt : restStack ->
            map (\rule -> case rhsNTRule rule of
                    [] -> ParseStep Predict rule (restStack, input)  -- if empty, dont change stack
                    (x : rest) -> ParseStep Predict rule (map Bar rest ++ [NoBar (lhs rule)] ++ restStack, input))  -- add rhs rest to the stack 
                (filter (\rule -> case rule of  -- Filter the NTRule where y == nt, and lhs /= nt
                    NTRule lhs (y : rest) -> y == nt && lhs /= nt  
                    _ -> False) rules)
        _ -> []

-- Connects two symbols when top of stack matches the starts of a NTRule 
connectLC :: (Eq nt, Eq t) => [RewriteRule nt t] -> Config nt t -> [ParseStep nt t]
connectLC rules (stack, input) =
    case stack of
        NoBar a : Bar b : restStack ->  -- Check if top two elements in stack are NoBar a and Bar b
            map (\rule -> case rhsNTRule rule of  -- Create ParseStep for matching NTRule
                    [] -> ParseStep Connect rule (restStack, input)  -- if empty, leave stack unchanged
                    (x : rest) -> ParseStep Connect rule (map Bar rest ++ restStack, input))  -- add remaining symbols to the stack
                (filter (\rule -> case rule of  -- Filter rules where rhs starts with the top stack elements.  
                                    NTRule lhs (y : rest) -> lhs == b && y == a  
                                    _ -> False) rules)
        _ -> []

-- Implements leftCorner parsing
leftCorner :: (Eq nt, Eq t) => CFG nt t -> [t] -> [[ParseStep nt t]]
leftCorner cfg input =
    let (nts, ts, start, rules) = cfg in
    let startingConfig = ([Bar start], input) in
    let goalConfig = ([], []) in
    parser [shiftLC, predictLC, matchLC, connectLC] rules startingConfig goalConfig




-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                               Tester Code

-- Custom Test CFG: 
cfgCustom1 :: CFG Cat String
cfgCustom1 =   ([S, NP, VP, PP, D, N, V, P], 
               ["girl", "boy", "the", "on", "met", "Mary", "ate", "paella", "in", "Spain"], 
               S, 
               [ (NTRule S [NP, VP]),        
                 (NTRule NP [D, N]),        
                 (NTRule VP [V, NP]),        
                 (NTRule VP [V, NP, PP]),   
                 (NTRule PP [P, NP]),          
                 
                 (TRule D "the"),
                 (TRule N "girl"),
                 (TRule N "boy"),
                 (TRule NP "paella"),
                 (TRule NP "Spain"),
                 (TRule NP "Mary"),
                 (TRule V "ate"),
                 (TRule V "met"),
                 (TRule P "in"),
                 (TRule P "on")
               ])


cfgCustom2 :: CFG Cat String
cfgCustom2 = ([S, NP, VP, D, N, V, ORC],  
              ["the", "aardvark", "panda", "chased", "slept"],  
              S,  
              [ (NTRule S [NP, VP]),    
                (NTRule NP [D, N]),             
                (NTRule NP [D, N, ORC]),    
                (NTRule ORC [NP, V]),       
                (NTRule VP [V]),            
                (NTRule VP [V, NP]),

                -- Terminal rules
                (TRule D "the"),
                (TRule N "aardvark"),
                (TRule N "panda"),
                (TRule V "chased"),
                (TRule V "slept")
              ])


-------------------------------------------------------------------------------
--         Bottom-Up Tests:
-------------------------------------------------------------------------------
-- bottomUp cfg4 (words "the baby saw the boy")

        -- [===== BEGIN PARSE =====
        -- NoTransition   NoRule        ([],["the","baby","saw","the","boy"])
        -- Shift          D -> "the"    ([D],["baby","saw","the","boy"])
        -- Shift          N -> "baby"   ([D,N],["saw","the","boy"])
        -- Reduce         NP -> D N     ([NP],["saw","the","boy"])
        -- Shift          V -> "saw"    ([NP,V],["the","boy"])
        -- Shift          D -> "the"    ([NP,V,D],["boy"])
        -- Shift          N -> "boy"    ([NP,V,D,N],[])
        -- Reduce         NP -> D N     ([NP,V,NP],[])
        -- Reduce         VP -> V NP    ([NP,VP],[])
        -- Reduce         S -> NP VP    ([S],[])
        -- ===== END PARSE =====
        -- ]


-- bottomUp cfgCustom (words "the girl ate paella in Spain")

        -- [===== BEGIN PARSE =====
        -- NoTransition   NoRule           ([],["the","girl","ate","paella","in","Spain"])
        -- Shift          D -> "the"       ([D],["girl","ate","paella","in","Spain"])
        -- Shift          N -> "girl"      ([D,N],["ate","paella","in","Spain"])
        -- Reduce         NP -> D N        ([NP],["ate","paella","in","Spain"])
        -- Shift          V -> "ate"       ([NP,V],["paella","in","Spain"])
        -- Shift          NP -> "paella"   ([NP,V,NP],["in","Spain"])
        -- Shift          P -> "in"        ([NP,V,NP,P],["Spain"])
        -- Shift          NP -> "Spain"    ([NP,V,NP,P,NP],[])
        -- Reduce         PP -> P NP       ([NP,V,NP,PP],[])
        -- Reduce         VP -> V NP PP    ([NP,VP],[])
        -- Reduce         S -> NP VP       ([S],[])
        -- ===== END PARSE =====
        -- ]


-- bottomUp cfgCustom2 (words "the aardvark the panda chased slept")

        -- [===== BEGIN PARSE =====
        -- NoTransition   NoRule            ([],["the","aardvark","the","panda","chased","slept"])
        -- Shift          D -> "the"        ([D],["aardvark","the","panda","chased","slept"])
        -- Shift          N -> "aardvark"   ([D,N],["the","panda","chased","slept"])
        -- Shift          D -> "the"        ([D,N,D],["panda","chased","slept"])
        -- Shift          N -> "panda"      ([D,N,D,N],["chased","slept"])
        -- Reduce         NP -> D N         ([D,N,NP],["chased","slept"])
        -- Shift          V -> "chased"     ([D,N,NP,V],["slept"])
        -- Reduce         ORC -> NP V       ([D,N,ORC],["slept"])
        -- Reduce         NP -> D N ORC     ([NP],["slept"])
        -- Shift          V -> "slept"      ([NP,V],[])
        -- Reduce         VP -> V           ([NP,VP],[])
        -- Reduce         S -> NP VP        ([S],[])
        -- ===== END PARSE =====
        -- ]


-------------------------------------------------------------------------------
--         Top-Down Tests: 
-------------------------------------------------------------------------------
--(NOTE: comment out (NTRule NP [NP,POSS,N]) from cfg4, so that it doesn't loop infinitely)
-- topDown cfg4 (words "the baby saw the boy")    

        -- [===== BEGIN PARSE =====
        -- NoTransition   NoRule        ([S],["the","baby","saw","the","boy"])
        -- Predict        S -> NP VP    ([NP,VP],["the","baby","saw","the","boy"])
        -- Predict        NP -> D N     ([D,N,VP],["the","baby","saw","the","boy"])
        -- Match          D -> "the"    ([N,VP],["baby","saw","the","boy"])
        -- Match          N -> "baby"   ([VP],["saw","the","boy"])
        -- Predict        VP -> V NP    ([V,NP],["saw","the","boy"])
        -- Match          V -> "saw"    ([NP],["the","boy"])
        -- Predict        NP -> D N     ([D,N],["the","boy"])
        -- Match          D -> "the"    ([N],["boy"])
        -- Match          N -> "boy"    ([],[])
        -- ===== END PARSE =====
        -- ]


-- topDown cfgCustom (words "the girl ate paella in Spain")

        -- [===== BEGIN PARSE =====
        -- NoTransition   NoRule           ([S],["the","girl","ate","paella","in","Spain"])
        -- Predict        S -> NP VP       ([NP,VP],["the","girl","ate","paella","in","Spain"])
        -- Predict        NP -> D N        ([D,N,VP],["the","girl","ate","paella","in","Spain"])
        -- Match          D -> "the"       ([N,VP],["girl","ate","paella","in","Spain"])
        -- Match          N -> "girl"      ([VP],["ate","paella","in","Spain"])
        -- Predict        VP -> V NP PP    ([V,NP,PP],["ate","paella","in","Spain"])
        -- Match          V -> "ate"       ([NP,PP],["paella","in","Spain"])
        -- Match          NP -> "paella"   ([PP],["in","Spain"])
        -- Predict        PP -> P NP       ([P,NP],["in","Spain"])
        -- Match          P -> "in"        ([NP],["Spain"])
        -- Match          NP -> "Spain"    ([],[])
        -- ===== END PARSE =====
        -- ]


-- topDown cfgCustom2 (words "the aardvark the panda chased slept")

        -- [===== BEGIN PARSE =====
        -- NoTransition   NoRule            ([S],["the","aardvark","the","panda","chased","slept"])
        -- Predict        S -> NP VP        ([NP,VP],["the","aardvark","the","panda","chased","slept"])
        -- Predict        NP -> D N ORC     ([D,N,ORC,VP],["the","aardvark","the","panda","chased","slept"])
        -- Match          D -> "the"        ([N,ORC,VP],["aardvark","the","panda","chased","slept"])
        -- Match          N -> "aardvark"   ([ORC,VP],["the","panda","chased","slept"])
        -- Predict        ORC -> NP V       ([NP,V,VP],["the","panda","chased","slept"])
        -- Predict        NP -> D N         ([D,N,V,VP],["the","panda","chased","slept"])
        -- Match          D -> "the"        ([N,V,VP],["panda","chased","slept"])
        -- Match          N -> "panda"      ([V,VP],["chased","slept"])
        -- Match          V -> "chased"     ([VP],["slept"])
        -- Predict        VP -> V           ([V],["slept"])
        -- Match          V -> "slept"      ([],[])
        -- ===== END PARSE =====
        -- ]


-------------------------------------------------------------------------------
--         Left-Corner Tests:
-------------------------------------------------------------------------------
-- leftCorner cfg4 (words "the baby saw the boy")

        -- [===== BEGIN PARSE =====
        -- NoTransition   NoRule        ([S*],["the","baby","saw","the","boy"])
        -- Shift          D -> "the"    ([D S*],["baby","saw","the","boy"])
        -- Predict        NP -> D N     ([N* NP S*],["baby","saw","the","boy"])
        -- Match          N -> "baby"   ([NP S*],["saw","the","boy"])
        -- Connect        S -> NP VP    ([VP*],["saw", "the","boy"])
        -- Shift          V -> "saw"    ([V VP*],["the", "boy"])
        -- Connect        VP -> V NP    ([NP*],["the", "boy"])
        -- Shift          D -> "the"    ([D NP*],["boy"])
        -- Connect        NP -> D N     ([N*],["boy"])
        -- Match          N -> "boy"    ([],[])
        -- ===== END PARSE =====
        -- ]


-- leftCorner cfgCustom (words "the girl ate paella in Spain")

        -- [===== BEGIN PARSE =====
        -- NoTransition   NoRule           ([S*],["the","girl","ate","paella","in","Spain"])
        -- Shift          D -> "the"       ([D,S*],["girl","ate","paella","in","Spain"])
        -- Predict        NP -> D N        ([N*,NP,S*],["girl","ate","paella","in","Spain"])
        -- Match          N -> "girl"      ([NP,S*],["ate","paella","in","Spain"])
        -- Connect        S -> NP VP       ([VP*],["ate","paella","in","Spain"])
        -- Shift          V -> "ate"       ([V,VP*],["paella","in","Spain"])
        -- Connect        VP -> V NP PP    ([NP*,PP*],["paella","in","Spain"])
        -- Match          NP -> "paella"   ([PP*],["in","Spain"])
        -- Shift          P -> "in"        ([P,PP*],["Spain"])
        -- Connect        PP -> P NP       ([NP*],["Spain"])
        -- Match          NP -> "Spain"    ([],[])
        -- ===== END PARSE =====
        -- ]


-- leftCorner cfgCustom2 (words "the aardvark the panda chased slept")

        -- [===== BEGIN PARSE =====
        -- NoTransition   NoRule            ([S*],["the","aardvark","the","panda","chased","slept"])
        -- Shift          D -> "the"        ([D,S*],["aardvark","the","panda","chased","slept"])
        -- Predict        NP -> D N ORC     ([N*,ORC*,NP,S*],["aardvark","the","panda","chased","slept"])
        -- Match          N -> "aardvark"   ([ORC*,NP,S*],["the","panda","chased","slept"])
        -- Shift          D -> "the"        ([D,ORC*,NP,S*],["panda","chased","slept"])
        -- Predict        NP -> D N         ([N*,NP,ORC*,NP,S*],["panda","chased","slept"])
        -- Match          N -> "panda"      ([NP,ORC*,NP,S*],["chased","slept"])
        -- Connect        ORC -> NP V       ([V*,NP,S*],["chased","slept"])
        -- Match          V -> "chased"     ([NP,S*],["slept"])
        -- Connect        S -> NP VP        ([VP*],["slept"])
        -- Shift          V -> "slept"      ([V,VP*],[])
        -- Connect        VP -> V           ([],[])
        -- ===== END PARSE =====
        -- ]