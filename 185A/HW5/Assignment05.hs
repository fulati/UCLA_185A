{-# LANGUAGE FlexibleInstances #-}

module Assignment05 where

import Control.Applicative(liftA, liftA2, liftA3)

import SemiringFSA

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.


--Question 1: 
--A
backward :: (Semiring v) => GenericAutomaton st sy v -> [sy] -> st -> v
backward m w q = 
    let (states, syms, i, f, delta) = m in
    case w of
    [] -> f q
    (x:rest) -> gen_or (map (\q1 -> delta (q,x,q1) &&& backward m rest q1) states)


--B
f :: (Semiring v) => GenericAutomaton st sy v -> [sy] -> v
f m w =
    let (states, syms, i, f, delta) = m in
    gen_or (map (\q -> i q &&& backward m w q) states)


--Question 2: 
--C
addCosts :: Cost -> Cost -> Cost
addCosts (TheInt x) (TheInt y) = TheInt (x + y)
addCosts _ Inf = Inf
addCosts Inf _ = Inf


--D 
minCost :: Cost -> Cost -> Cost
minCost (TheInt x) (TheInt y) = TheInt (min x y)
minCost x Inf = x 
minCost Inf y = y


--E
instance Semiring Cost where
    x &&& y = addCosts x y
    x ||| y = minCost x y
    gtrue = TheInt 0
    gfalse = Inf


--Question 3: 
--F
instance Semiring [[a]] where
    x &&& y = concat (map (\u -> map (\v -> u ++ v) y) x)
    x ||| y = x ++ y
    gtrue = [[]]
    gfalse = []


--G
gfsa34 :: GenericAutomaton Int Char [[Char]]
gfsa34 = makeGFSA [] ([1,2,3], ['C','V'],
                       [(1, [[]])], [(1, [[]])], 
                       [((1,'V',1), ["V"]),
                        ((1,'C',2), ["C"]),
                        ((1,'V',3), ["V"]),
                        ((2,'V',1), ["V", "VV"]),
                        ((2,'V',3), ["V", "VV"]),
                        ((3,'C',1), [[]])])


--H
gfsa_flap :: GenericAutomaton Int Char [[Char]] 
gfsa_flap = makeGFSA [] ([1,2,3], ['a','n','t'],
                       [(1, [[]])], [(1, [[]]), (2, [[]]), (3, ["t"])],
                       [((1,'n',1), ["n"]),
                        ((1,'t',1), ["t"]),
                        ((1,'a',2), ["a"]),
                        ((2,'n',1), ["n"]),
                        ((2,'a',2), ["a"]),
                        ((2,'t',3), [[]]),
                        ((3,'a',2), ["ta", "Ta"]),
                        ((3,'n',1), ["tn"]),
                        ((3,'t',1), ["tt"])])


--Question 4:
--I
gfsa5_count :: GenericAutomaton Int Char Double
gfsa5_count = makeGFSA 0 ([1,2,3], ['C','V'],
                         [(1, 1.0)], 
                         [(1, 1.0)], 
                         [((1,'V',1), 1.0),
                          ((1,'C',2), 1.0),
                          ((1,'V',3), 1.0),
                          ((2,'V',1), 1.0),
                          ((2,'V',3), 1.0),
                          ((3,'C',1), 1.0)])


--J
gfsa5_paths :: GenericAutomaton Int Char [[Int]]
gfsa5_paths = makeGFSA [] ([1,2,3], ['C','V'],
                          [(1, [[]])], 
                          [(1, [[1]])], 
                          [((1,'V',1), [[1]]),
                           ((1,'C',2), [[1]]),
                           ((1,'V',3), [[1]]),
                           ((2,'V',1), [[2]]),
                           ((2,'V',3), [[2]]),
                           ((3,'C',1), [[3]])])

