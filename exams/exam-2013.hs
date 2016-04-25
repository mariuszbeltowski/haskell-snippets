-- ex 2013, 1
oddBins :: Int -> [[Int]]
oddBins n = filter (\l-> (mod (sum l) 2 ) == 1) (ciagi n)

ciagi :: Int -> [[Int]]
ciagi 1 = [[0], [1]]
ciagi n = (map (0:) $ ciagi (n-1) ) ++ (map (1:) $ ciagi (n-1) )

-- ex 2013, 2

diffsums2 list = diffsums $ reverse list

diffsums :: [[Int]] -> [[Int]]
diffsums list = [a1 | (x,y)<-(sums list), not (elem x [ z1 | (z1,z2)<-(drop y (sums list))]), (a1,a2)<-(numbered list), a2==y ]

sums :: [[Int]] -> [(Int,Int)]
sums list = zip [sum x | x<-list] [1..]

numbered :: [[Int]] -> [([Int], Int)]
numbered list = zip list [1..]

-- ex 2013, 3
compref :: Eq a => [a] -> [a] -> Int
compref (x:xs) (y:ys) = if x==y then 1+compref xs ys else 0
compref _ _ = 0
