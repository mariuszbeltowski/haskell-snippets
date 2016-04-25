-- fibonacci
fib 0 = 1
fib 1 = 1
fib n = fib (n-1)+fib (n-2)

-- merge sort
merge a [] = a
merge [] b = b
merge (x:xs) (y:ys) = if x<y
  then x:(merge xs (y:ys))
  else y:(merge ys (x:xs))

msort [] = []
msort [x] = [x]
msort xs = merge (msort as) (msort bs)
  where
    (as, bs) = split xs

split [] = ([], [])
split [x] = ([x], [])
split (x:y:xys) = (x:xs, y:ys)
  where
    (xs, ys) = split xys


-- quick sort
quicksort [] = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser = filter (< p) xs
        greater = filter (>= p) xs

-- sort
sort :: (Ord a) => [a] -> [a]
sort = foldr insert []

insert x [] = [x]
insert x (y:ys) = if x <= y then x : y : ys else y : insert x ys
