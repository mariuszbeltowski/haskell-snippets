-- ex 2014, 1
numocc :: Eq a => a -> [[a]] -> [Int]
numocc el list = [ length $ filter (\e-> e == el)  x  | x<-list ]

-- ex 2014, 2
data CT a = Empty | Leaf a | Join (CT a) Op (CT a)

wf :: CT a -> Bool
wf Empty = False
wf (Leaf a) = True
wf (Join l op r) = wf l == True && wf r == True

data Op = Add | Mul | Neg deriving (Eq)


eval :: Num a => CT a -> a
eval (Leaf a) = a
eval n@(Join l op r)
    | not (wf n) = error "Wrong tree"
    | op == Add = eval l + eval r
    | op == Mul = eval l * eval r
    | op == Neg = eval l - eval r


-- ex 2014, 3
h :: [a] -> [a]
h list = [fst x | x <- ( filter (\x-> even $ snd x)  (zip list [0..]))]

-- ex 3 linear
h2 list = h2pom list 0
h2pom [] _ = []
h2pom (x:xs) c = if even c then x:h2pom xs (c+1) else h2pom xs (c+1)
