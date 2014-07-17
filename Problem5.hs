-- Smallest positive number this is evenly divisible by all numbers from 1 to 20
module Problem5 where

main = print $ solve 20

solve :: Int -> Int
solve x = evenly_divisible x [11..x]
    where
        evenly_divisible 500000000 divisors = -1
        evenly_divisible start divisors
            | num_sum == 0 = start
            | otherwise    = evenly_divisible (start+20) divisors
            where 
                num_sum = sum $ map (\x -> start `mod` x) divisors
