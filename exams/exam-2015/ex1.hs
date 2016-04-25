
f :: [Double] -> Double
f = foldr ((.(1/)).(+)) 1
