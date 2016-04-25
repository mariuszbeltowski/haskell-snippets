-- ex 2012, 1
subsets2 :: [Int] -> [[Int]]
subsets2 []  = [[]]
subsets2 (x:xs) = subsets2 xs ++ map (x:) (subsets2 xs)

subsets :: [Int] -> [[Int]]
subsets list = concat [reverse $  filter (\a -> length a == x) (subsets2 list) | x<-[0..(length list)] ]

-- ex 2012, 3
equalPairs :: Eq a => [a] -> [a] -> Int
equalPairs list1 list2 = length $ filter (\(x,y)-> x==y) (zip list1 list2)
