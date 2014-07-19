-- Find product of pythagorean triple (a^2 + b^2 = c^2) such that a + b + c = 1000
module Problem9 where

main = print solve

solve :: Int
solve = 
    let 
        triple:ts = filter pyTriplet $ genLists 1000.0
    in
        product $ map truncate triple 

pyTriplet :: [Float] -> Bool
pyTriplet (a:b:c:ds) = a + b + c == 1000.0

genLists :: Float -> [[Float]]
genLists max = [[x, y, sqrt $ x^2+y^2] | x <- [1.0..max], y <- [x..max]]
