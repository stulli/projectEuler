main :: IO ()
main = print $ snd $ maximum $ map (\(res,x)->(length res,x)) $ zip (map (\x -> [(a,b,x-a-b) | a <- [1..(div x 2)], b <- [1..a], a*a+b*b==(x-a-b)*(x-a-b)]) [1..1000]) [1..]
