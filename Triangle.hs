module Triangle (triangle, triangles) where

-- get the n-th triangle number
triangle n = sum $ take n [1..]

-- get a list of all triangle numbers
triangles = scanl1 (+) [1..]
