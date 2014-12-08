module Main where

import Data.Char (digitToInt)

highesNumber = 999999

isCoolNumber :: Int -> Bool
isCoolNumber n = n == (sum $ map (\x -> (digitToInt x)^4) $ show n)

main = print $ sum $ filter isCoolNumber [2..highesNumber]
