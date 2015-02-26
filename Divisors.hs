module Divisors (divisors, allDivisors, isPrime, divisors') where

-- all divisors, including 1, but excluding the number itself
allDivisors :: Integer -> [Integer]
allDivisors x = 1: (divisors' x 2)

-- number of divisors, including 1 and the number itself
divisors :: Integer -> Integer
divisors x = fromIntegral $ length $ x:1: (divisors' x 2)

divisors' :: Integer -> Integer -> [Integer]
divisors' x y
    | y > div x y = []
    | mod x y == 0 = y: (divisors' x (y+1))
    | otherwise = divisors' x (y+1)

isPrime :: Integer -> Bool
isPrime n
    | divisors n == 2 = True
    | otherwise = False
