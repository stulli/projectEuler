import System.IO (readFile)
import Data.Char (digitToInt)

main = do
    content <- readFile "eu08.input"
    let numbers = map digitToInt $ filter (/= '\n') content
    let result = maximum $ calculate numbers
    print $ result

calculate :: [Int] -> [Int]
calculate l
    | length l < 13 = []
    | otherwise = (product $ take 13 l) : calculate (tail l)
