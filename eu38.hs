module Pandigital where

import Data.List (nub, sort)

panDigit :: Int -> [String]
panDigit num = panFilter $ takeWhile (\x -> length x < 10) $ multiplyStrings num

panFilter :: [String] -> [String]
panFilter = filter (\x -> length x == 9 && length (nub x) == 9 && '0' `notElem` x)

multiplyStrings :: Int -> [String]
multiplyStrings num = map (\x -> concatMap (show . (*num)) [1..x]) [2..]

main :: IO ()
main = putStrLn $ head $ last $ sort $ filter (not . null) $ map panDigit [1..9999]
