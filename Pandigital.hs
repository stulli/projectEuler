module Pandigital where

import Data.List (nub, sort)

isPanDigit :: Integer -> Bool
isPanDigit num = sort x == concatMap show (take (length x) [1..]) && '0' `notElem` x
    where x = show num

-- for euler 38
panDigit :: Int -> [String]
panDigit num = panFilter $ takeWhile (\x -> length x < 10) $ multiplyStrings num

panFilter :: [String] -> [String]
panFilter = filter (\x -> length x == 9 && length (nub x) == 9 && '0' `notElem` x)

multiplyStrings :: Int -> [String]
multiplyStrings num = map (\x -> concatMap (show . (*num)) [1..x]) [2..]
