isqrt = floor . sqrt . fromIntegral
 
primes = 2:3:[x | x <- [5,7..], primeTest x (isqrt x) (tail primes)]
 
primeTest n sqrt_n (d:ds) = if d > sqrt_n then True 
                            else if (mod n d)==0 then False
                                 else primeTest n sqrt_n ds
 
-- problem 7
main :: IO ()
main = print $ primes !! 10000 -- 10001st prime number

-----------------------------------------------------------------
indexOflpbm = indexOflpbm' 0 primes
indexOflpbm' c (x:xs) = if x > 1000000 then c
                        else indexOflpbm' (c+1) xs
 
--hello = sum $ takeWhile (<1000000) $ primes
answer = foldl (+) 0 (take indexOflpbm primes)
