-- ex 8.1,2
data Calk = Zero | Pop Calk | Nast Calk

toInt :: Calk -> Int
toInt Zero = 0
toInt (Nast x) = 1 + toInt x
toInt (Pop x) = (toInt x ) - 1


instance Show Calk where
    show x = "N: " ++ show (toInt x)


-- ex 8.3
data MCalk = M [[Int]]

instance Show MCalk where
    show (M[]) = " "
    show (M(x:xs)) = (listToStr x) ++ "\n" ++ (show(M xs))


listToStr :: [Int] -> String
listToStr [] = " "
listToStr (x:xs) = (show x) ++ " " ++  listToStr xs

(+++) :: MCalk -> MCalk -> MCalk
(+++) (M m1) (M m2) = M(map (uncurry dodaj)(zip m1 m2))

dodaj :: [Int] -> [Int] -> [Int]
dodaj x y = map (uncurry(+)) (zip x y)

-- M([[1,2], [3,4]]) +++ M([[2,2],[3,4]])

-- ex 8.4
data BT a = Empty | Node a (BT a) (BT a) deriving Show

isEmpty :: Eq a => (BT a) -> Bool
isEmpty Empty = True
isEmpty _ = False

size :: (BT a) -> Integer
size Empty = 0
size (Node a l1 l2) = 1 + size l1 + size l2

depth :: (BT a) -> Integer
depth Empty = 0
depth (Node a l1 l2) = 1 + max (depth l1) (depth l2)

-- ex 8.5
insert :: (Ord a) => a -> (BT a) -> (BT a)
insert a Empty = (Node a Empty Empty)
insert a (Node x l r)
    | a > x = Node x l (insert a r)
    | otherwise = Node x (insert a l) r

-- ex 8.6
ctree :: (Ord a) => [a] -> (BT a)
ctree [] = Empty
ctree (x:xs) = ctree2 (Node x Empty Empty) xs
    where
        ctree2 tr [] = tr
        ctree2 tr (x:xs) = ctree2 (insert x tr) xs

inorder :: (Ord a) => (BT a) -> [a]
inorder Empty = []
inorder (Node v t1 t2) = inorder t1 ++ [v] ++ inorder t2

tsort :: (Ord a) => [a] -> [a]
tsort list = inorder $ ctree list

-- ex 8.7
instance Eq MCalk where
    (M m1) == (M m2) = mcalkEq m1 m2

mcalkEq :: [[Int]] -> [[Int]] -> Bool
mcalkEq m1 m2 = sum2d m1 == sum2d m2

sum2d :: [[Int]] -> Int
sum2d list = sum [ sum x | x <- list ]

instance Ord MCalk where
    (M m1) <= (M m2) = sum2d m1 <= sum2d m2
