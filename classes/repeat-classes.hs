-- ex 1
part x = part2 1 x
    where
        part2 _ 0 = [[]]
        part2 i x = [n:d | n<-[i..x], d <-part2 n (x-n)]

-- ex 2

data BT a=Empt | Nod a (BT a) (BT a) deriving Show

preorder :: (BT a) -> [a]
preorder Empt = []
preorder (Nod n t0 t1) = [n] ++ preorder t0 ++ preorder t1

inorder :: (BT a) -> [a]
inorder Empt = []
inorder (Nod n t0 t1) = inorder t0 ++ [n] ++ inorder t1

postorder :: (BT a) -> [a]
postorder Empt = []
postorder (Nod n t0 t1) = postorder t0 ++ postorder t1 ++ [n]


--  ex 3
splitD :: String -> String -> Char -> [String]
splitD [] tmp _ = [tmp]
splitD (x:xs) tmp d = if x==d then (tmp:splitD xs [] d) else splitD xs (tmp++[x]) d

split :: String -> [String]
split (x:xs) = splitD xs [] x

-- ex 4

evenSubsets n = filter evenFilter (subsets [1..n])

subsets :: [Int] -> [[Int]]
subsets []  = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

evenFilter x = (sum x) `mod` 2 == 0 && length x > 0


--  ex 5
data Expr = N Double
    | Dodawanie Expr Expr
    | Odejmowanie Expr Expr
    | Mnozenie Expr Expr
    | Dzielenie Expr Expr

eval :: Expr -> Double
eval (N n) = n
eval (Dodawanie a b) = eval a + eval b
eval (Odejmowanie a b) = eval a - eval b
eval (Mnozenie a b) = eval a * eval b
eval (Dzielenie a b) = eval a / eval b

-- use:
-- (1.0 - 2.0) * 3.0
-- Mnozenie( Odejmowanie (N 1.0) (N 2.0)) (N 3.0)

-- ex 6
oddBins :: Int -> [[Int]]
oddBins n = filter (\l-> (mod (sum l) 2 ) == 1) (ciagi n)

ciagi :: Int -> [[Int]]
ciagi 1 = [[0], [1]]
ciagi n = (map (0:) $ ciagi (n-1) ) ++ (map (1:) $ ciagi (n-1) )

-- ex6 try 2
bins :: Int -> [[Int]]
bins 0 = [[]]
bins n = concat [[0:x]++[1:x] | x <-(bins (n-1))]
oddBins2 :: Int -> [[Int]]
--oddBins2 = filter (odd.sum).bins
oddBins2 n = filter(\x -> odd $ sum x) (bins n)

-- ex 8
data Tree a = Empty | Node (Tree a) a (Tree a)
treeHelp :: (Tree a) -> Int
treeHelp Empty = -1
treeHelp (Node l x r) = 1+min (treeHelp l) (treeHelp r)

treeLev Empty _ = []
treeLev t@(Node t1 x t2) n = treeLev t1 n ++ (if treeHelp t==n then [x] else []) ++ (treeLev t2 n)

-- ex 9
f :: [a] -> [[[a]]]
f xs = iterate genLists [[x] | x <- xs]
    where
       genLists yss = [ x : ys | x <- xs , ys <- yss]


--  ex 10
data Matrix a = M[[a]] deriving Show
(***) :: (Num a) => (Matrix a) -> (Matrix a) -> (Matrix a)
(***) (M a) (M b) = M(multiply a (transpose b))

-- ?? might be [] = [] (maybe)
transpose ([]:_) = []
transpose list = map (\e->head e) list : transpose (map tail list)

multiply [] _ = []
multiply (x:xs) y = (multiplyRow x y) : multiply xs y
    where
        multiplyRow _ [] = []
        multiplyRow x (y:ys) = sum (zipWith(*) x y ) : multiplyRow x ys

-- ex 11

data Zespolona = Zesp Double Double

instance Num Zespolona where
    (+) (Zesp a1 i1) (Zesp a2 i2) = Zesp (a1+a2) (i1+i2)
    (-) (Zesp a1 i1) (Zesp a2 i2) = Zesp (a1-a2) (i1-i2)
    (*) (Zesp a1 i1) (Zesp a2 i2) = Zesp (a1*a2) (i1*i2)
    fromInteger i = Zesp (fromIntegral i) 0
    abs (Zesp a1 i1) = Zesp a1 i1
    signum (Zesp a1 i1) = Zesp a1 i1
