n x
    | even x = div x 2
    | otherwise = 3*x + 1

f :: Integer -> [Integer]
f 1 = []
f x = n x : f (n x)

g = maximum $ map (\x -> (length $ f x, x)) [1..9999]

main = putStrLn $ show g
