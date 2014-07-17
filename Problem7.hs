module Problem7 where

main = print $ solve 10001

solve :: Int -> Int
solve x = nthPrime x 1 0

nthPrime :: Int -> Int -> Int -> Int
nthPrime total x acc
    | total == acc = x-1
    | otherwise    = if isPrime x
                        then nthPrime total (x+1) (acc+1)
                        else nthPrime total (x+1) acc

isPrime :: Int -> Bool
isPrime n = 
    let
        root = ceiling . sqrt . fromIntegral
        factors = filter (\x -> n `mod` x == 0) [2..root n]
    in
        null factors
