a=[[x, a, b] | x <- [1..292], a <- [x+1..499], b <- [1000-a-x], a^2+x^2==b^2]

main :: IO ()
main = print $ product $ head a
