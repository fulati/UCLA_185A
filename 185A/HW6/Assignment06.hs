{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
{-# LANGUAGE FlexibleInstances #-}

module Assignment06 where

import Control.Applicative(liftA, liftA2, liftA3)

import ContextFree

data Tree nt t = Leaf nt t | NonLeaf nt (Tree nt t) (Tree nt t) deriving Show

tree1 :: Tree Cat String
tree1 = NonLeaf VP (NonLeaf VP (Leaf V "watches") (Leaf NP "spies"))
                   (NonLeaf PP (Leaf P "with") (Leaf NP "telescopes"))

tree2 :: Tree Cat String
tree2 = NonLeaf VP (Leaf V "watches")
                   (NonLeaf NP (Leaf NP "spies") (NonLeaf PP (Leaf P "with") (Leaf NP "telescopes")))
------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.
                 

-- Question 1: 
--A: 
leftDeriv :: Tree nt t -> [RewriteRule nt t]
leftDeriv (Leaf nt t) = [TRule nt t]
leftDeriv (NonLeaf nt (Leaf lnt lt) (Leaf rnt rt)) = 
    [NTRule nt (lnt, rnt)] ++ [TRule lnt lt] ++ [TRule rnt rt]
leftDeriv (NonLeaf nt (Leaf lnt lt) (NonLeaf rnt rltree rrtree)) = 
    [NTRule nt (lnt, rnt)] ++ [TRule lnt lt] ++ leftDeriv (NonLeaf rnt rltree rrtree)
leftDeriv (NonLeaf nt (NonLeaf lnt lltree lrtree) (Leaf rnt rt)) = 
    [NTRule nt (lnt, rnt)] ++ leftDeriv (NonLeaf lnt lltree lrtree) ++ [TRule rnt rt]
leftDeriv (NonLeaf nt (NonLeaf lnt lltree lrtree) (NonLeaf rnt rltree rrtree)) = 
    [NTRule nt (lnt, rnt)] ++ leftDeriv (NonLeaf lnt lltree lrtree) ++ leftDeriv (NonLeaf rnt rltree rrtree)




--B: 
f :: (Ord nt, Ord t, Semiring a) => GenericCFG nt t a -> [t] -> a
f cfg str = 
    let (nts, ts, initial, r) = cfg in
    gen_or (map (\n -> initial n &&& fastInside cfg str n) nts)




