module Workshop5 where

import Data.Char
import Data.List

data Blt a = Leaf a | Node (Blt a) (Blt a) deriving (Eq,Show)

exampleTree :: Blt String
exampleTree = Node (Node (Leaf "Hoare, Tony")
                         (Leaf "Turing, Alan"))
                   (Leaf "Goedel, Kurt")

data Tree a = T a [Tree a] deriving (Eq,Ord,Show)

example0 = T 0 []
example1 = T 1 [T 2 [], T 3 []]
example2 = T 0 [example1,example1,example1]

depth :: Tree a -> Int 
depth (T _ []) = 0
depth (T _ ts) = foldl max 0 (map depth ts) + 1

foldT :: (a -> [b] -> b) -> Tree a -> b
foldT f (T x ts) = f x (map (foldT f) ts)

grow :: (node -> [node]) -> node -> Tree node 
grow step seed = T seed (map (grow step) (step seed))

tree n = grow (f n) (1,1)

f n = \ (x,y) -> if x+y <= n then [(x,x+y),(x+y,y)] else []


