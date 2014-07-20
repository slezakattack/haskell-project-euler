-- find sum of primes under 2 million
-- Note: This program runs slowly. Not too sure of an easier way right now.
module Problem10 where
main = print solve

solve :: Integer
solve = sum $ filter isPrime [2..2000000]

isPrime :: Integer -> Bool
isPrime 2 = True
isPrime n = 
    let
        root = ceiling . sqrt . fromIntegral
        factors = filter (\x -> n `mod` x == 0) [2..root n]
    in
        null factors
