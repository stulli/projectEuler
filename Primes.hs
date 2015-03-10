module Primes (primes, primfaktoren, divisors) where

import Data.List (subsequences, nub)

-- all prime numbers
primes = 2:3:[x | x <- [5,7..], primeTest x (isqrt x) (tail primes)]
 
primeTest n sqrt_n (d:ds) = if d > sqrt_n then True 
                            else if (mod n d)==0 then False
                                 else primeTest n sqrt_n ds

isqrt = floor . sqrt . fromIntegral

---------------------------------------------------
-- list all prime factors of x
primfaktoren x = primfaktoren' x (takePrimes x)

takePrimes n = takeWhile (<=(div n 2)) $ primes

primfaktoren' _ [] = []
primfaktoren' x (y:ys)
    | mod x y == 0 = y:primfaktoren' (div x y) (y:ys)
    | otherwise = primfaktoren' x ys

-- list all divisors of x
-- maybe not very fast, better use allDivisors from Divisors module
divisors x = nub $ map product $ subsequences $ primfaktoren x 

-- get all circular primes of a given prime (eu 35)
--circularPrimes :: Int -> [Int]
--circularPrimes p = map (\x -> read x::Int) $ concat $ [ [x] | x <- show p ]
