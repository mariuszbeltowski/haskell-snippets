-- ex 6.1
sqList :: [Int] -> [Int]
-- sqList l = map (^2) l
sqList l = [x*x | x <- l]

-- ex 6.2
sumList :: [[Int]] -> Int -> [Int]
sumList lists k = [sum x | x<-lists, length x > k]

-- ex 6.3
dividers :: Int -> [Int]
dividers n = [x | x<-[1..n], mod n x == 0]

-- ex 6.3.2
getPrimes :: Int -> [Int]
getPrimes n = [x | x<-[2..n], dividers x == [1,x] ]

-- ex 6.4
app :: Int -> [Int] -> [[Int]]
app n list = [(take x list) ++ [n] ++ (take ((length list)-x) (reverse list)) | x<-[0..(length list)]]

apply :: Int -> [[Int]] -> [[Int]]
apply n list = concat [app n x | x <- list]

perm :: Int -> [[Int]]
perm 1 = [[1]]
--perm 2 = [[1,2],[2,1]]
perm n = apply n (perm (n-1))

-- ex 6.5
subsets :: Int -> [a] -> [[a]]
subsets 0 _ = [[]]
subsets _ [] = []
subsets k (x:xs) = map (x:) (subsets (k - 1) xs) ++ subsets k xs

subset :: Int -> Int -> [[Int]]
subset n k = subsets k [1..n]

-- ex 6.6
sieve :: Int -> [Int] -> [Int]
sieve n ns = filter (notDivBy n) ns
      where notDivBy n k = (k `mod` n) /= 0

eratos :: [Int] -> [Int]
eratos (n:ns) = n : eratos (sieve n ns)
eratos []     = []

primes = eratos [2..]

--
-- list2perm
list2perm [] = []
list2perm [x] = [(1,x)]
list2perm xs = list2permn xs 1

list2permn [] n = []
list2permn (x:xs) n = [(n, x)] ++ list2permn xs (n+1)
