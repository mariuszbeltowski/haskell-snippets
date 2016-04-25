-- Thue–Morse sequence
-- ex 7.1
f :: Char -> String
f 'a' = "ab"
f 'b' = "ba"

thu :: String -> String
thu str = concatMap(\x -> f x) str

thuemorse :: [String]
thuemorse = iterate thu "a"

thuemorseN :: Int -> String
thuemorseN n = last $ take n thuemorse

-- ex 7.2
remove :: Eq a => [a] -> a -> [a]
remove list ele = [ x | x <- list, x /= ele ]

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser = filter (< p) xs
        greater = filter (>= p) xs


-- ex 7.3
encList :: Eq a => [a] -> [(a, Int)]
encList [] = []
encList (x:xs) = [(x, 1+(count x xs))] ++ encList (drop ((count x xs)) xs)

count :: Eq a => a -> [a] -> Int
count _ [] = 0
count var (x:xs) = if var == x then 1 + (count var xs) else 0

-- ex 7.4
f1 (x,y) = uncurry (+) (x,y)
f2 x = uncurry (*) (x,x)
f3 x y = ( uncurry (==) (x,y) )

-- ex 7.5
mniejszeOd :: Ord a => [a] -> [a] -> Int
mniejszeOd xs ys = sum $ map (uncurry (\x y -> if x<y then 1 else 0) ) $ zip xs ys

--mniejsze list1 list2 = length(A:iter(\x->x==True)(map(\x->(uncurry(<))x)(zip list1 list2)))


-- ex 7.6
ef :: Int -> Int
ef 0 = 1
ef 1 = 3
ef n = (2* ef (n-1)) - ((ef (n-2))^2)

eg :: [Int] -> [Int]
eg list = map (uncurry (\x y-> 2*(ef (x-1))-((ef (y-2))^2) ) $  zip (tail list) list


fy = eg [2..]
