module Main where

import Divisors (isPrime)
import Data.List (inits,tails)

primes = filter isPrime [10..]

isFunnyPrime p = all isPrime $ map read $ filter (/="") $ inits (show p) ++ tails (show p)

main :: IO ()
main = print $ sum $ take 11 $ filter isFunnyPrime [10..]
