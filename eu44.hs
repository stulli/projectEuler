import Data.List (sort)

pnumberss :: [Float]
pnumberss = map (fromIntegral . p) [1..4000] -- 4000 was just a guess

p n = n * (3 * n - 1) `div` 2

isPent :: Float -> Bool
isPent x | (snd $ properFraction  $ (sqrt (24*x+1) + 1) / 6) == 0.0 = True
         | otherwise = False
    
test :: [Float]
test = [(y-x) | x <- pnumberss,
                y <- filter (>x) pnumberss,
                isPent (y-x),
                isPent (x+y)
                ]

main :: IO ()
main = print $ head $ sort test
