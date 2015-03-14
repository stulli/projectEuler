import Data.List.Split (splitOn)
import Data.Char (ord)

isTriangle :: Int -> Bool
isTriangle x = half * (half + 1) == x*2
    where half = floor $ sqrt $ (fromIntegral x*2)

wordValue :: String -> Int
wordValue w = sum $ map (\x -> ord x - 64) w

main :: IO ()
main = do
    content <- readFile "eu42.input"
    let words = splitOn "," $ filter (/='"') content
        wordValues = map wordValue words
    print $ length $ filter isTriangle wordValues

