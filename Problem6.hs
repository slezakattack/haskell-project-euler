module Problem6 where

main = print $ solve 100

solve :: Int -> Int
solve x = a - b
    where 
        a = (^2) $ sum [1..x] 
        b = sum $ map (^2) [1..x]
