import System.IO

main = do
    content <- readFile "eu18.input"
    let pyramid = map (map read . words) $ lines content
    calculate pyramid

calculate :: [[Int]] -> Int
calculate x:xs = 
