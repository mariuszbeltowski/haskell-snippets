-- longer :: Eq a => [a] -> (a, Int)
-- 3
reverse2 :: [a] -> [a]
reverse2 = foldl (flip (:)) []

-- 4
perfect :: Integer -> Bool
perfect n = sum [x | x<-[1..(n-1)], mod n x == 0 ] == n


-- 2
dictionary :: [String] -> [(String, Int)]
dictionary list = quicksort $ iterateit (distinct list) list

quicksort :: [(String, Int)] -> [(String, Int)]
quicksort [] = []
quicksort ((key, count):xs) = (quicksort lesser) ++ [(key, count)] ++ (quicksort greater)
    where
        lesser = filter (\(a,b) -> b <count) xs
        greater = filter ( \(a,b) -> b >= count) xs

iterateit :: [String] -> [String] -> [(String, Int)]
iterateit [] _ = []
iterateit (x:xs) list = [(x, (countit list x))] ++ iterateit xs list

countit :: [String] -> String -> Int
countit [] _ = 0
countit (x:xs) key =
  if (x == key) then
    1 + countit xs key
  else
    countit xs key

distinct :: (Eq a) => [a] -> [a]
distinct [] = []
distinct (x:xs) = if elem x xs then distinct xs else x:distinct xs
