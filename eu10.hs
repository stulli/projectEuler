import System.IO
import Data.Char

main = do
    content <- readFile "primes1.txt"
    let primes = map read $ words content
    let result = sum primes
    putStrLn $ show result

