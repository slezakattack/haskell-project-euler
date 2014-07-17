-- find sum of all multiples of 3 and 5 up to 100.
module Problem1 where
main = print $ sum $ filter (\x -> x`mod` 3 == 0 || x `mod` 5 == 0) [1..999]
