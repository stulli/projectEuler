fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main :: IO ()
main = print $ fst $ head $ dropWhile not1000 $ zip [0..] fibs
    where not1000 (_,x) = (length $ show x) /= 1000
