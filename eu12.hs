import Data.List

triangle n = sum $ take n [1..]

primes :: [Int] -> [Int]
primes (x:[]) = [x]
primes (x:xs) = x : primes [y | y <- xs, mod y x /= 0]

takePrimes n = takeWhile (<=(div n 2)) $ primes [2..]

-- primeFactors x = filter (\y -> mod x y == 0) $ takePrimes x

divisors x = nub $ map product $ subsequences $ primfaktoren x 

primfaktoren x = primfaktoren' x (takePrimes x)

primfaktoren' _ [] = []
primfaktoren' x (y:ys)
    | mod x y == 0 = y:primfaktoren' (div x y) (y:ys)
    | otherwise = primfaktoren' x ys

-- f x = f' x [1..x]

-- f' x (y:ys) 
--    | = map (length . divisors) $  map triangle [1..x]
