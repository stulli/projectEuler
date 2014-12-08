module Main where

import Factorial (fac)
import Data.Char (digitToInt)

sumOfFacs n = sum $ map (fac . digitToInt) $ show n

property n = n == sumOfFacs n

f = filter property [145..2540160]

main = print f
