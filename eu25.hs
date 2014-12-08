module Main where

import Data.Maybe
import Data.List 
import Data.List.Ordered as LO

--main = print $ moreFilter abList initialList
main = print $ mainPrint -- $ sum $ initialList \\ sumAbundants abList

mainPrint = (sum [1..limit]) - (sum $ LO.nub $ sort $ concat test)

test = map (\x -> map (+x) (fst (partition (<=limit-x) (fst (partition (>=x) abList))) ) ) abList

-- abList resultList
moreFilter :: [Integer] -> [Integer] -> [Integer]
moreFilter [] ys = ys
moreFilter (x:xs) ys = moreFilter xs $ filterList x ys (x:xs)

    
-- input: 1 abundant number and current list of non abundant numbers
-- then calculates all sums with this number and returns a new list
-- with all non abundant numbers
filterList :: Integer -> [Integer] -> [Integer] -> [Integer]
filterList a nonAbList remainingAbundantList = x ++ (y \\ map (+a) remainingAbundantList)
    where
    tuple = partition (>=a) nonAbList
    y = fst tuple
    x = snd tuple

limit = 28123
--limit = 1000

initialList = [1..limit]

abList = allAbundants limit

allAbundants :: Integer -> [Integer]
allAbundants x = map fromJust $ filter (/= Nothing) $ map abundant [1..x]
    
abundant x
    | x < sumDivisors x = Just x
    | otherwise = Nothing

-- input: abList
sumAbundants :: [Integer] -> [Integer]
sumAbundants [] = []
sumAbundants l@(x:xs) = map (+x) ll `Data.List.union` sumAbundants xs 
    where
    ll = fst $ partition (<= limit - x) l

sumDivisors = sum . allDivisors

allDivisors :: Integer -> [Integer]
allDivisors x = 1: (divisors' x 2)

divisors :: Integer -> Integer
divisors x = fromIntegral $ length $ x:1: (divisors' x 2)

divisors' :: Integer -> Integer -> [Integer]
divisors' x y 
    | y > div x y = []
    | y == div x y && mod x y == 0 = [div x y]
    | mod x y == 0 = (div x y):y: (divisors' x (y+1))
    | otherwise = divisors' x (y+1)

