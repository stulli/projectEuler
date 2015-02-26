module Main where

isPalin n = n == (read $ reverse $ show n)

base2 :: Integer -> String
base2 0 = ""
base2 n
    | odd n = base2 (n `div` 2) ++ "1"
    | otherwise = base2 (n `div` 2) ++ "0"

f = sum $ filter (\x -> isPalin x && (isPalin $ base2 x)) [1..1000000]

main :: IO ()
main = print f
