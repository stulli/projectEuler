import Data.List
f = length $ nub $ permutations $ replicate 10 0 ++ replicate 10 1
main = putStrLn $ show f
eqPerms [] = [[]]
eqPerms xs = [x:xt | x <- nub xs, xt <- eqPerms $ delete x xs]

