module Problem2 where
main = print solve

solve :: Int
solve = sum $ filter even (fibb [2,1])

fibb :: [Int] -> [Int]
fibb (a:b:bs)
    | a < 4000000  = fibb ([a + b] ++ [a] ++ [b] ++ bs)
    | otherwise = [a] ++ [b] ++ bs
