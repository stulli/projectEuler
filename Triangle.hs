module Triangle (triangle, triangles, isTriangle, isPent, isHexa, hexas, pents) where

-- get the n-th triangle number
triangle n = sum $ take n [1..]

-- get a list of all triangle numbers
triangles = scanl1 (+) [1..]

-- test if a number is a triangle number
isTriangle :: Integer -> Bool
isTriangle x = half * (half + 1) == x * 2
    where half = floor $ sqrt $ fromIntegral $ x * 2

-- all pentagonal numbers
pents = map (\x -> x * (3*x - 1) `div` 2) [1..]

-- test if a number is a pentagonal number
isPent :: Integer -> Bool
isPent x = (snd $ properFraction $ (sqrt (24*y+1) + 1) / 6) == 0.0
    where y = fromIntegral x

-- all hexagonal numbers
hexas = map (\x -> x * (2*x - 1)) [1..]

-- test if a number is a hexagonal number
isHexa :: Integer -> Bool
isHexa x = (snd $ properFraction $ ((2 * sqrt (2*y+1)) + 1) / 4) <= 0.1
    where y = fromIntegral x
