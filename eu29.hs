module Main where

import Data.List (nub)

main = print . length . nub $ combos

combos = [a^b | a <- [2..100], b <- [2..100] ]
