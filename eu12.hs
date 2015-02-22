import Triangle (triangle, triangles)
import Primes (divisors)

main :: IO ()
main = print $ head $ dropWhile (\x -> snd x < 200) $ map (\x -> (x, length $ divisors x)) triangles
