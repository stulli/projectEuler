import System.IO

main = do
    content <- readFile "input"
    let result = map (map read . words) $ lines content
    putStrLn $ show (result::[[Int]])
