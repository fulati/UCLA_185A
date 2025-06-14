module Assignment04 where

import Prelude hiding (Either(..))

import Control.Applicative(liftA, liftA2, liftA3)
import Data.List(nub)

import FiniteStatePart2

---------------------------------------
-- Setup for section 1

type SLG sy = ([sy], [sy], [sy], [(sy,sy)])
data ConstructedState sy = ExtraState | StateForSymbol sy deriving (Eq, Show)

slg1 :: SLG SegmentCV
slg1 = ([C,V], [C], [V], [(C,C),(C,V),(V,V)])

slg2 :: SLG Int
slg2 = ([1,2,3], [1,2,3], [1,2,3], [(1,1),(2,2),(3,3),(1,2),(2,1),(1,3),(3,1)])

---------------------------------------
-- Setup for section 2

data Either a b = First a | Second b deriving (Show,Eq)

re1 :: RegExp Char
re1 = Concat (Alt (Lit 'a') (Lit 'b')) (Lit 'c')

re2 :: RegExp Char
re2 = Star re1

re3 :: RegExp Int
re3 = Star (Concat ZeroRE (Lit 3)) 

re4 :: RegExp Int
re4 = Concat (Alt (Lit 0) (Lit 1)) (Star (Lit 2))

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.



-- Question 1
-- A: generatesSLG
backwardSLG :: (Eq sy) => SLG sy -> [sy] -> sy -> Bool
backwardSLG m w q = 
    let (syms, i, f, bigrams) = m in 
    case w of 
        [] -> elem q f
        (x:rest) -> or (map (\(s1, s2) -> s1 == q && s2 == x && backwardSLG m rest x) bigrams)


generatesSLG :: (Eq sy) => SLG sy -> [sy] -> Bool
generatesSLG m w = 
    case w of 
        [] -> False
        (x:rest) -> let (syms, i, f, bigrams) = m in 
                    elem x i && backwardSLG m rest x


-- C: slgToFSA
slgToFSA :: SLG sy -> Automaton (ConstructedState sy) sy
slgToFSA m = 
    let (syms, initialSyms, finalSyms, bigrams) = m in 
    let states = ExtraState : map StateForSymbol syms in 
    let i = [ExtraState] in
    let f = map StateForSymbol finalSyms in 
    let initialTransition = map (\x -> (ExtraState, x, StateForSymbol x)) initialSyms in 
    let validTransition = map (\(a, b) -> (StateForSymbol a, b, StateForSymbol b)) bigrams in 
    let delta = initialTransition ++ validTransition in 

    (states, syms, i, f, delta)




-- Question 2
-- F: unionFSAs
unionFSAs :: (Eq sy) => EpsAutomaton st1 sy -> EpsAutomaton st2 sy -> EpsAutomaton (Either st1 st2) sy
unionFSAs (states, syms, i, f, delta) (states', syms', i', f', delta') =
    let newStates = (map First states) ++ (map Second states') in
    let newSyms = nub (syms ++ syms') in
    let newI = (map First i) ++ (map Second i') in
    let newF = (map First f) ++ (map Second f') in
    let newDelta = (map (\(x, y, z) -> (First x, y, First z)) delta) ++ 
                   (map (\(x, y, z) -> (Second x, y, Second z)) delta') in

    (newStates, newSyms, newI, newF, newDelta)


-- G: concatFSAs
concatFSAs :: (Eq sy) => EpsAutomaton st1 sy -> EpsAutomaton st2 sy -> EpsAutomaton (Either st1 st2) sy
concatFSAs (states, syms, i, f, delta) (states', syms', i', f', delta') =
    let newStates = (map First states) ++ (map Second states') in
    let newSyms = nub (syms ++ syms') in
    let newI = (map First i) in
    let newF = (map Second f') in
    let newDelta = (map (\(x, y, z) -> (First x, y, First z)) delta) ++ 
                   (map (\(x, y, z) -> (Second x, y, Second z)) delta') ++ 
                   concat (map (\finalState -> map (\initialState -> (First finalState, Nothing, Second initialState)) i') f) in

    (newStates, newSyms, newI, newF, newDelta)


-- H: starFSA
starFSA :: EpsAutomaton st sy -> EpsAutomaton (Either Int st) sy
starFSA (states, syms, i, f, delta) = 
    let newStates = First 0 : map Second states in
    let newSyms =  syms in
    let newI = [First 0] in
    let newF = First 0 : map Second f in
    let newDelta = (map (\initialState -> (First 0, Nothing, Second initialState)) i) ++ 
                   (map (\(x, y, z) -> (Second x, y, Second z)) delta) ++ 
                   concat (map (\finalState -> map (\initialState -> (Second finalState, Nothing, Second initialState)) i) f) in

    (newStates, newSyms, newI, newF, newDelta)


-- I: flatten
flatten :: Either Int Int -> Int
flatten (First a) = 2 * a    -- Even numbers
flatten (Second b) = 2 * b + 1    --Odd numbers


-- J: mapStates
mapStates :: (a -> b) -> EpsAutomaton a sy -> EpsAutomaton b sy
mapStates func (states, syms, i, f, delta) = 
    let newStates = map func states in
    let newSyms =  syms in
    let newI = map func i in
    let newF = map func f in
    let newDelta = (map (\(x, y, z) -> (func x, y, func z)) delta) in
                   
    (newStates, newSyms, newI, newF, newDelta)


-- K: reToFSA
reToFSA :: (Eq sy) => RegExp sy -> EpsAutomaton Int sy
reToFSA regexp = case regexp of
        Lit x -> ([0, 1], [x], [0], [1], [(0, Just x, 1)])
        Alt r1 r2 -> mapStates flatten (unionFSAs (reToFSA r1) (reToFSA r2))
        Concat r1 r2 -> mapStates flatten (concatFSAs (reToFSA r1) (reToFSA r2))
        Star r -> mapStates flatten (starFSA (reToFSA r))
        ZeroRE -> ([0], [], [0], [], [])     
        OneRE -> ([0, 1], [], [0], [1], [(0, Nothing, 1)])
