pnumbers :: [Float]
pnumbers = map (fromIntegral . p) [24000..25000]

pnumberss :: [Float]
pnumberss = map (fromIntegral . p) [1..400]

p n = n * (3 * n - 1) `div` 2

-- ohoh x = map (\x -> snd x - fst x) x

isPent :: Float -> Bool
isPent x | (snd $ properFraction  $ (sqrt (24*x+1) + 1) / 6) == 0.0 = True
         | otherwise = False
    

test :: [Float]
test = [(y-x) | x <- pnumbers, 
                y <- filter (>x) pnumbers, 
--                z <- filter (>y) pnumbers, 
--                x+y==z,
                isPent (y-x),
                isPent (x+y)
                ]

run = filter (<5408352) test                

main = return run
