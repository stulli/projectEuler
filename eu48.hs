main = putStr $ show $ mod (foldr (\x -> (+) (x^x)) 0 [1..1000]) (10^10)
