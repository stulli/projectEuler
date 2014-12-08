module Main where

import Data.List (genericLength)

main = do
    content <- readFile "primzahlen1mio.txt"
    let primes = map read $ words content :: [Integer]
    print $ filter (f primes) primes

f :: [Integer] -> Integer -> Bool
f primes prime = f' primes (genericLength $ show prime) prime

f' :: [Integer] -> Integer -> Integer -> Bool
f' primes anz prime
    | anz < 0 = True
    | prime `elem` primes = f' primes (anz -1) $ rotate $ show prime
    | otherwise = False

rotate :: String -> Integer
rotate (h:xs) = read $ xs ++ [h]
