-- pointless
-- f x y = 3*x + y
-- f x y = (+) 3*x y
-- f x =(+) 3*x
-- f x = (+) (*(3) x)
-- f = (+).((*)3)

-- f x y = 3*x/y
-- f x y = (/) 3*x y
-- f x = (/) 3*x
-- f x = (/) ((*)3) x
-- f = (/).((*)3)

-- exam 2013/3 compref
compref::[Int]->[Int]->Int
compref = ((.)((length.(takeWhile(==True))).(map (uncurry (==))))).zip


numocc::Int -> [[Int]]-> [Int]
numocc = map.((length.).(filter.(==)))

-- exam 2013 ex1
select :: Int  -> [[a]] -> [a]
select = map.((flip (!!)).((+)(-1)))
