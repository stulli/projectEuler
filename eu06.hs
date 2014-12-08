
difference = squareofsums - sumofsqares where
    sumofsqares = foldr (\x -> (+) (x * x)) 0 [1..100]
    squareofsums = (sum [1..100])^2
