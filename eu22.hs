import Data.List (sort)
import Data.Char (ord)

main = do
    content <- readFile "eu22.input"
    let result = sum $ 
                 zipWith (*) [1..] $ 
                 map (sum . map (\x -> ord x - ord 'A' + 1)) $
                 sort $ words $ 
                 map (\x -> if x == ',' then ' ' else x) $ 
                 filter (/= '\"') content
    print result
