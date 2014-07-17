-- Find largest prime factor of 600,851,475,143
module Problem3 where
main = print $ solve 2 600851475143

solve :: Integer -> Integer -> Integer
solve i j 
    | j == i         = j
    | j `mod` i == 0 = solve 2 (j `div` i)
    | j `mod` i /= 0 = solve (i+1) j
    | otherwise      = -1                  -- We should never hit this. I know that -1 would never be an answer.
