import Pandigital (isPanDigit)
import Primes (primes)

main :: IO ()
-- 8 and 9 digits do not work as sum [1..8] and sum [1..9] are divisible by 3.
-- Since I didn't implement a proper primarily test I needed to test every
-- number up to 7654321 instead of counting down...
main = print $ maximum $ filter isPanDigit $ takeWhile (<7654321) primes
