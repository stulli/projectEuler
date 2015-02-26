fibs ::  [Integer]
fibs = 1 : 1 : (zipWith (+) fibs (tail fibs))

main ::  IO ()
main = do
    print $ sum $ filter (\x -> x `mod` 2 == 0) (takeWhile (<4000000) fibs)
