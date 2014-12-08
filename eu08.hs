import System.IO
import Data.Char

main = do
    content <- readFile "input"
    let stream = map digitToInt $ filter (/= '\n') content
    let result = maximum $ calculate stream
    putStrLn $ show result

calculate :: [Int] -> [Int]
calculate (_:_:_:_:[]) = []
calculate (a:b:c:d:e:xs) = (a*b*c*d*e) : calculate (b:c:d:e:xs)
