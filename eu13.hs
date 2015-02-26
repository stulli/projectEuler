import System.IO

main = do
    content <- readFile "eu13.input"
    putStrLn $ compute content
    where
        compute = take 10 . show . sum . map read . lines
