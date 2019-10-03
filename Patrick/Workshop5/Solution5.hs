import Workshop5
import Data.Char
import Data.List

-- Question 1
leafCount :: Blt a -> Int
leafCount (Leaf a) = 1
leafCount (Node l r) = leafCount l + leafCount r

-- Question 2
mapB :: (a -> b) -> Blt a -> Blt b
mapB f (Leaf v) = Leaf (f v)
mapB f (Node l r) = Node (mapB f l) (mapB f r)

-- Question 3
count :: Tree a -> Int
count (T v l) = 1 + sum (map count l)

-- Question 4

-- Question 5
mapT :: (a -> b) -> Tree a -> Tree b
mapT f (T v l) =T (f v) (map (mapT f) l)

-- Question 7
collect :: Tree a -> [a]
collect (T v l) = v : concatMap (collect) l

-- Question 8
foldCount :: Tree a -> Int
foldCount t = foldT (\ x xs -> sum xs + 1) t

foldDepth :: Tree a -> Int
foldDepth t = (foldT (\ x xs -> 1 + listMax xs ) t) - 1
    where listMax [] = 0
          listMax l = maximum l

foldCollect :: Tree a -> [a]
foldCollect t = foldT (\x ys -> x : concat ys) t

foldMapT :: (a -> b) -> Tree a -> Tree b 
foldMapT f t = foldT (\x ys -> T (f x) ys) t 

-- Question 9
-- Because the tree grows exponentially (every node gets two child nodes) we have:
-- 2^(lim+1) - 1 where lim is the value in the step function
-- because lim=6, we have 2^7 - 1 = 127

-- Question 10
infTree :: Tree Integer
infTree = grow (\ n -> [n+1,n+1]) 0

takeT :: Int -> Tree a -> Tree a
takeT 0 (T v xs) = T v []
takeT l (T v xs) = T v (map (takeT (l-1)) xs)

-- Question 11


