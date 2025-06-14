module Assignment02 where

-- Imports everything from the Recursion module. 
import Recursion

-- Imports just a few things that we have seen from the standard Prelude module. 
-- (If there is no explicit 'import Prelude' line, then the entire Prelude 
-- module is imported. I'm restricting things here to a very bare-bones system.)
import Prelude((+), (-), (*), (<), (>), (++), not, (||), (&&), Bool(..), Int, Show, Char, Eq)

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.


-- Question 1: 
------------------------------------------------------------------
-- A. Mulitply
multiply :: Numb -> (Numb -> Numb)
multiply = \n -> \m -> case n of
                       Z -> Z
                       S n' -> add m (multiply n' m)
                       
-- B. SumUpTo
sumUpTo :: Numb -> Numb
sumUpTo = \n -> case n of 
                Z -> Z
                S n' -> add (S n') (sumUpTo n')

-- C. NumToInt
numbToInt :: Numb -> Int
numbToInt = \n -> case n of
                  Z -> 0
                  S n' -> 1 + (numbToInt n')

-- D. Equals
equals :: Numb -> (Numb -> Bool)
equals = \n -> \m -> case n of 
                     Z -> case m of 
                          Z -> True
                          S m' -> False
                     S n' -> case m of 
                             Z -> False
                             S m' -> ((equals n') m')


-- Question 2: 
------------------------------------------------------------------
-- E. Count
count :: (a -> Bool) -> ([a] -> Numb)
count = \f -> \l -> case l of 
                    [] -> Z
                    x : rest -> case (f x) of 
                                True -> S (count f rest)
                                False -> (count f rest)

-- F. ListOf
listOf :: Numb -> (a -> [a]) 
listOf = \n -> \x -> case n of 
                     Z -> []
                     S n' -> x : listOf n' x

-- G. AddToEnd
addToEnd :: a -> ([a] -> [a])
addToEnd = \x -> \l -> case l of 
                       [] -> [x]
                       y : rest -> y : addToEnd x rest

-- H. Remove
remove :: (a -> Bool) -> ([a] -> [a])
remove = \f -> \l -> case l of 
                     [] -> []
                     x : rest -> case (f x) of 
                                 True -> (remove f rest)
                                 False -> x : (remove f rest)

-- I. Prefix
prefix :: Numb -> ([a] -> [a])
prefix = \n -> \l -> case l of
                      [] -> []
                      x : rest -> case n of 
                                  Z -> []
                                  S n' -> x : prefix n' rest

-- J. Reverse
reverse :: [a] -> [a]
reverse = \l -> case l of 
                [] -> []
                x : rest -> addToEnd x (reverse rest)


-- Question 3: 
------------------------------------------------------------------
-- K. CountStars
countStars :: RegExp a -> Numb
countStars = \r -> case r of 
                   Star r' -> S (countStars r')
                   Alt r1 r2 -> add (countStars r1) (countStars r2)
                   Concat r1 r2 -> add (countStars r1) (countStars r2)
                   Lit r1 -> Z
                   ZeroRE -> Z
                   OneRE -> Z

-- L. Depth
depth :: RegExp a -> Numb
depth = \r -> case r of 
              Lit x -> S Z
              Star r' -> S (depth r')
              Alt r1 r2 -> S (bigger (depth r1) (depth r2))
              Concat r1 r2 -> S (bigger (depth r1) (depth r2))
              ZeroRE -> S Z
              OneRE -> S Z

-- Question 4: 
------------------------------------------------------------------
-- M. Nice Patterns

-- (a) is equivalent to ϕ
-- (b) is equivalent to F
-- (c) is equivalent to ϕ
-- (d) is equivalent to ϕ

-- (e) is equivalent to r
-- (f) is equivalent to 0
-- (g) is equivalent to r
-- (h) is equivalent to r

-- In both the formulas and the regular expressions, I saw that they handle disjunction and conjunction similarly.
-- In the Formulas, "V" works similar to the regular expressions's "|", where having a "F" or a "0" doesn't effect the value for ϕ and r.
-- And the "∧" works similar to the regular expression's "·", where having a "F" or "0" can change the result, and "T" or "1" keeps the value same for ϕ and r.
-- One food for thought I noticed was that there is no negation for the regular expression.
