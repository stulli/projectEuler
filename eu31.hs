module Main where

import Prelude hiding (pred, succ)
import Data.List (nub)

goal = 200

succ x
    | x == 200 = 0
    | x == 100 = 200
    | x == 50 = 100
    | x == 20 = 50
    | x == 10 = 20
    | x == 5 = 10
    | x == 2 = 5
    | x == 1 = 2

pred x
    | x == 200 = 100
    | x == 100 = 50
    | x == 50 = 20
    | x == 20 = 10
    | x == 10 = 5
    | x == 5 = 2
    | x == 2 = 1
    | x == 1 = 0

nextListFrom :: [Integer] -> [Integer]
nextListFrom [] = []
nextListFrom (h:[]) = if pred h > 0 then [pred h] else []
nextListFrom old = reverse $ nextListFrom' $ reverse old

nextListFrom' (h:[])
    | pred h /= 0 = [pred h]
    | otherwise = []
nextListFrom' (h:n:xs)
    | pred h /= 0 = (pred h):n:xs
    | otherwise = nextListFrom' $ n:xs

newElementFrom :: [Integer] -> Integer
newElementFrom [] = undefined   -- should never happen
newElementFrom l@(h:_)
    | sum l + h <= goal = h 
    | otherwise = pred h

processList :: [Integer] -> [[Integer]]
processList [] = [[]]
processList l
    | sum l == goal = l:processList (nextListFrom l)
    | sum l > goal = processList (nextListFrom l)
    | otherwise = processList $ l ++ reverse [newElementFrom (reverse l)]


f :: Integer 
  -> [[Integer]]  -- list of coin combinations where their sum = 200
f p = processList [p]

main = print $ (length $ f goal) - 1    -- -1 wegen leerer Liste am Ende
