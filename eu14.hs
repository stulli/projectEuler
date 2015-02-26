n x
    | even x = div x 2
    | otherwise = 3*x + 1

-- compute Collatz sequence
f :: Integer -> [Integer]
f 1 = []
f x = n x : f (n x)

g = maximum $ map (\x -> (length $ f x, x)) [1..999999]

main = print $ snd g
