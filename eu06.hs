
difference maxNumber = squareofsums - sumofsqares where
    sumofsqares = foldr (\x -> (+) (x * x)) 0 [1..maxNumber]
    squareofsums = (sum [1..maxNumber])^2

main :: IO ()
main = print $ difference 100
