module Main where

import Data.List (genericLength)
import Primes (primes)

main = do
    let primes' = takeWhile (<1000000) primes
    print $ show $ length primes'
    let primes'' = filter (f primes') primes'
    -- forgot to account for zeros :)
    let primes''' = filter (\x -> '0' `notElem` show x) primes''
    print $ length primes'''

f :: [Integer] -> Integer -> Bool
f primes prime = f' primes (genericLength $ show prime) prime

f' :: [Integer] -> Integer -> Integer -> Bool
f' primes anz prime
    | anz < 0 = True
    | prime `elem` primes = f' primes (anz -1) $ rotate $ show prime
    | otherwise = False

rotate :: String -> Integer
rotate (h:xs) = read $ xs ++ [h]

-- about 2 minutes, interpreted
-- TODO: use proper combinatorial algorithm
