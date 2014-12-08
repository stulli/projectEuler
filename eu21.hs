module Main where

main = do
    print $ sum $ map (\(x,y,z) -> x) $ filter (\(x,y,z) -> x==z && x/=y) $ map (\x -> (x, sumDivisors x, sumDivisors (sumDivisors x))) [1..9999]

sumDivisors :: Integer -> Integer
sumDivisors = sum . allDivisors

allDivisors :: Integer -> [Integer]
allDivisors x = 1: (divisors' x 2)

divisors' :: Integer -> Integer -> [Integer]
divisors' x y
     | y >= div x y = []
     | mod x y == 0 = (div x y):y: (divisors' x (y+1))
     | otherwise = divisors' x (y+1)

