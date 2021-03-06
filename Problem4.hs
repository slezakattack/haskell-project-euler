-- Find largest palindrome generated by multiplying two 3-digit numbers.
module Problem4 where

main = print solve

solve :: Int
solve = maximum $ filter isPalindrome [a*b | a <- [100..999], b <- [100..999]]

isPalindrome :: Int -> Bool
isPalindrome x = compare (show x) (reverse $ show x)
    where 
        compare [] [] = True
        compare (x:xs) (y:ys)
            | x /= y    = False
            | x == y    = compare xs ys
            | otherwise = False
