import Data.List (nub, sort)
import Pandigital (panDigit)


main :: IO ()
main = putStrLn $ head $ last $ sort $ filter (not . null) $ map panDigit [1..9999]
