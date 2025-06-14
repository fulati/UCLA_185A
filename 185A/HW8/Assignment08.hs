{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}

module Assignment08 where

import Control.Applicative(liftA, liftA2, liftA3)

import TreeGrammars

plainWords = ["John","Mary","ate","bought","an","apple","books","yesterday","C","laughed","because"]
whWords = ["who","what","why"]
qWords = ["Q"]

------------------------------------------------------
-- Some tiny helpers for writing trees more compactly

lf :: a -> Tree a
lf x = Node x []

mrg :: Tree String -> Tree String -> Tree String
mrg t1 t2 = Node "*" [t1,t2]

------------------------------------------------------

-- (1a)/(2a) `C John ate an apple yesterday'
tree_1a :: Tree String
tree_1a = mrg (lf "C") (mrg (lf "John") (mrg (mrg (lf "ate") (mrg (lf "an") (lf "apple"))) (lf "yesterday")))

-- (1b)/(2b) `Q John ate what yesterday'
tree_1b :: Tree String
tree_1b = mrg (lf "Q") (mrg (lf "John") (mrg (mrg (lf "ate") (lf "what")) (lf "yesterday")))

-- (3a) `Q John ate an apple yesterday'
tree_3a :: Tree String
tree_3a = mrg (lf "Q") (mrg (lf "John") (mrg (mrg (lf "ate") (mrg (lf "an") (lf "apple"))) (lf "yesterday")))

-- (3b) `C John ate what yesterday'
tree_3b :: Tree String
tree_3b = mrg (lf "C") (mrg (lf "John") (mrg (mrg (lf "ate") (lf "what")) (lf "yesterday")))

tree_13 :: Tree String
tree_13 =
    Node "*" [
        Node "Q" [],
        Node "*" [
            Node "John" [],
            Node "*" [
                Node "laughed" [],
                Node "**" [
                    Node "because" [],
                    Node "*" [
                        Node "Mary" [],
                        Node "*" [
                            Node "*" [Node "bought" [], Node "books" []],
                            Node "why" []
                        ]
                    ]
                ]
            ]
        ]
    ]

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

-- Question 1: 
--A: 
total :: (Eq a) => a -> Tree a -> Int
total x (Node y ts) = 
    case (x == y) of
        True  -> 1
        False -> 0
    + sum (map (total x) ts)


--B: 
leftmost :: Tree a -> [a]
leftmost (Node y ts) = 
    case ts of 
        [] -> [y]
        x : rest -> y : leftmost x


-- Question 2: 
--C: 
allLists :: Int -> [a] -> [[a]] 
allLists n l = 
    case n of 
        0 -> [[]]
        x -> concat (map (\y -> map (\z -> y : z) (allLists (n - 1) l )) l)


--D:
under :: (Eq st, Eq sy) => Automaton st sy -> Tree sy -> st -> Bool
under (st, sy, end, trans) (Node y ts) currState = 
    case ts of 
        [] -> elem ([], y, currState) trans
        x -> any (\z -> 
                elem (z, y, currState) trans &&
                all (\(t, s) -> under (st, sy, end, trans) t s) (zip ts z))
                (allLists (length ts) st)


--E:
generates :: (Eq st, Eq sy) => Automaton st sy -> Tree sy -> Bool
generates (st, sy, end, trans) (Node y ts) = 
    case end of 
        [] -> False
        (x : rest) -> 
            under (st, sy, end, trans) (Node y ts) x || 
            generates (st, sy, rest, trans) (Node y ts)


--Question 3: 
--F
data WhState = None | QState | WHState | CState | Q_WH | AState deriving (Show,Eq)

fsta_wh1 :: Automaton WhState String
fsta_wh1 = 
       ([None, QState, WHState, CState, Q_WH],
        plainWords ++ whWords ++ qWords ++ ["*"],
        [Q_WH],
        [
        ([None, None], "*", None), 
        ([QState, QState], "*", None),
        ([WHState, WHState], "*", WHState),

        ([QState, None], "*", None),
        ([None, QState], "*", None),
        ([QState, WHState], "*", Q_WH),

        ([None, WHState], "*", WHState),
        ([WHState, None], "*", WHState),

        ([], "C", CState),
        ([CState, None], "*", Q_WH)

        ] ++ map (\s -> ([], s, QState)) qWords 
          ++ map (\s -> ([], s, WHState)) whWords
          ++ map (\s -> ([], s, None)) plainWords
        )


--G
fsta_wh2 :: Automaton WhState String
fsta_wh2 = 
       ([None, QState, WHState, CState, Q_WH, AState],
        plainWords ++ whWords ++ qWords ++ ["*","**"],
        [Q_WH],
        [
        ([None, None], "*", None), 
        ([QState, QState], "*", None),
        ([WHState, WHState], "*", WHState),

        ([QState, None], "*", None),
        ([None, QState], "*", None),
        ([QState, WHState], "*", Q_WH),

        ([None, WHState], "*", WHState),
        ([WHState, None], "*", WHState),

        ([], "C", CState),
        ([CState, Q_WH], "*", Q_WH),
        ([Q_WH, CState], "*", Q_WH),
        
        ([None, AState], "**", AState),
        ([AState, None], "**", AState),
        ([AState, AState], "**", AState),

        ([QState, AState], "**", AState),
        ([AState, QState], "**", AState),
        ([QState, WHState], "**", Q_WH)

        ] ++ map (\s -> ([], s, QState)) qWords 
          ++ map (\s -> ([], s, WHState)) whWords
          ++ map (\s -> ([], s, None)) plainWords
        ) 
       
