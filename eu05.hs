import Control.Monad (msum)

-- 46189 is the multiple of all primes < 20
trylist = [46189, 92378 ..] -- only need to try multiples of 20, pointless otherwise

divisors = [18, 16, 15, 14, 12] -- filtered out unnecessary numbers

main :: IO ()
main = print $ msum $ map tryDiv trylist

tryDiv :: Integer -> Maybe Integer
tryDiv i = case all (==True) (map (\x -> isDivisableBy i x) divisors) of
                True -> Just i
                _    -> Nothing

isDivisableBy :: Integer -> Integer -> Bool
isDivisableBy i d = i `mod` d == 0

