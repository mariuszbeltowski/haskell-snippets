import Data.Maybe
-- ex 1

perm :: Ord a => [a] -> [[a]]
perm [x] = [[x]]
perm list = quicksort [ (head(drop(i-1) list)) : ogon | i <- [1..length(list)], ogon<- perm ((take (i-1) list)++(drop i list))]


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted

-- ex 2

is3 :: Int -> Maybe Int
is3 x
  --  | length [ x | x<-[(1.0) .. xdo3], x == pie] >= 1 = Just ( round (pie) :: Int )
    | elem pie [(1.0) .. xdo3] = Just ( round (pie) :: Int )
    | otherwise = Nothing
      where
        xdo3 = fromIntegral (x*x*x)
        pie =  (  (fromIntegral x)  **(1/3) )

c2 :: Maybe Int -> Int
c2 Nothing = 0
c2 (Just x) = x*2+1


c :: Maybe Int -> Int
c x
    | isJust x = fromJust x * 2 + 1
    | isNothing x = 0

fun :: Int -> Int
fun x = c $ (Just x) >>= is3
