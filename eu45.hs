import Triangle

main :: IO ()
main = print $ filter (\x -> isTriangle x && isPent x) hexas !! 2
