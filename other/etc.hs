
diffsums :: [[Int]] -> [[Int]]
diffsums list = [a1 | (x,y)<-(sums list), not (elem x [ z1 | (z1,z2)<-(drop y (sums list))]), (a1,a2)<-(numbered list), a2==y ]

sums :: [[Int]] -> [(Int,Int)]
sums list = zip ( foldl (\x y -> x ++ [sum y]) [] list ) [1..]

numbered :: [[Int]] -> [([Int], Int)]
numbered list = zip list [1..]

-- diffsums list = foldr (\x y -> if elem (sum x) [] )


myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:).f) []

-- myMap f = flip foldr [] ((.) (:) f)

myFilter             :: (a -> Bool) -> [a] -> [a]
myFilter p []        = []
myFilter p (x:xs)    = foldr (\x xs -> if p x then x:xs else xs ) [] xs

-- horner
value :: (Num a) => a -> [a] -> a
value x0 = foldl (\a b -> a*x0 +b) 0

-- equal pairs

filt p xs = foldr (\x xs -> if p x then x : xs else xs) [] xs

-- equalPairs :: Eq a => [a] -> [a] -> [(a,a,Int)]
equalPairs list1 list2 = map snd $ filt (\(x,y,z)-> x==y) (zip3 list1 list2 [1..])
