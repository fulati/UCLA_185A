module Assignment03 where

-- Imports everything from the FiniteState module
import FiniteState

-- A type we'll use as symbols for some FSAs
data SegmentPKIU = P | K | I | U | MB deriving (Show,Eq)

-- A list-like type that will be useful for computing forward values
data SnocList a = ESL | (SnocList a) ::: a deriving Show

-- The word ``hello'' encoded as a snoc list of characters
sl :: SnocList Char
sl = ((((ESL ::: 'h') ::: 'e') ::: 'l') ::: 'l') ::: 'o'

-- Checks that all states and symbols mentioned in the transition 
-- table (i.e. delta) come from the provided lists of states and symbols.
fsaSanityCheck :: (Eq a) => Automaton a -> Bool
fsaSanityCheck m =
    let (states, syms, i, f, delta) = m in
    let validTransition (q1,x,q2) = elem q1 states && elem x syms && elem q2 states in
    and (map validTransition delta)

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.



-- Question 1
-- A: fsa_countCs
fsa_countCs :: Automaton SegmentCV
fsa_countCs = ([20, 51, 13, 48], [C, V], [20], [48], [(20, V, 20), 
                                                      (20, C, 51), 
                                                      (51, V, 51), 
                                                      (51, C, 13), 
                                                      (13, V, 13),
                                                      (13, C, 20), 
                                                      (13, C, 48), 
                                                      (48, V, 48)])

-- Question 2
-- B: addToFront
addToFront :: a -> SnocList a -> SnocList a
addToFront x l = case l of 
                 ESL -> ESL ::: x
                 rest ::: y -> (addToFront x rest) ::: y

-- C: toSnoc
toSnoc :: [a] -> SnocList a
toSnoc l = case l of 
           [] -> ESL
           x : rest -> addToFront x (toSnoc rest)

-- D: forward
forward :: (Eq a) => Automaton a -> SnocList a -> State -> Bool
forward m w q = let (states, syms, i, f, delta) = m in 
                             case w of
                                ESL -> elem q i
                                rest ::: x -> or (map (\q0 -> elem (q0,x,q) delta && forward m rest q0) states)

-- E: generates2
generates2 :: (Eq a) => Automaton a -> [a] -> Bool
generates2 m w = let (states, syms, i, f, delta) = m in
                        or (map (\q0 -> elem q0 f && forward m (toSnoc w) q0) states)

-- Question 3
-- F: fsa_twoCs
fsa_twoCs :: Automaton SegmentCV
fsa_twoCs = ([1, 2, 3], [C, V], [1], [3], [(1, V, 1), 
                                                      (1, C, 2), 
                                                      (2, V, 2), 
                                                      (2, C, 3),
                                                      (3, C, 3), 
                                                      (3, V, 3)])

-- G: fsa_oddEven
fsa_oddEven :: Automaton SegmentCV
fsa_oddEven = ([1, 2, 3, 4], [C, V], [1], [2], [(1, C, 2), 
                                                      (1, V, 3),
                                                      (2, C, 1), 
                                                      (2, V, 4),
                                                      (3, V, 1),
                                                      (3, C, 4), 
                                                      (4, V, 2),
                                                      (4, C, 3)])

-- H: fsa_harmony
fsa_harmony :: Automaton SegmentPKIU
fsa_harmony = ([1, 2, 3], [P, K, I, U, MB], [1], [1, 2, 3], [(1, P, 1), 
                                                                  (1, K, 1),
                                                                  (1, MB, 1),
                                                                  (1, I, 2),
                                                                  (1, U, 3),
                                                                  (2, P, 2),
                                                                  (2, K, 2),
                                                                  (2, I, 2),
                                                                  (2, MB, 2),
                                                                  (3, P, 3),
                                                                  (3, K, 3),
                                                                  (3, U, 3),
                                                                  (3, MB, 3),
                                                                  (2, MB, 3),
                                                                  (3, MB, 2)])

-- I: fsa_adjacentMBU
fsa_adjacentMBU :: Automaton SegmentPKIU
fsa_adjacentMBU = ([1, 2, 3], [P, K, I, U, MB], [1], [1, 2], [(1, P, 1), 
                                                                  (1, K, 1),
                                                                  (1, MB, 1),
                                                                  (1, I, 2),
                                                                  (1, MB, 3),
                                                                  (2, P, 2),
                                                                  (2, K, 2),
                                                                  (2, I, 2),
                                                                  (2, MB, 2),
                                                                  (2, MB, 3),
                                                                  (3, MB, 2),
                                                                  (3, U, 1)])

-- Question 4
-- J: requireCs
requireCs :: Int -> Automaton SegmentCV
requireCs n =
    let states = [0 .. n] in
    let syms = [C,V] in
    let i = [0] in
    let f = [n] in
    let ctransitions = map (\x -> (x, C, x + 1)) [0 .. n - 1] in
    let vtransitions = map (\x -> (x, V, x)) [0 .. n] in
    (states, syms, i, f, ctransitions ++ vtransitions)



