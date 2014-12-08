
fibs ::  [Integer]
fibs = 1 : 1 : (zipWith (+) fibs (tail fibs))

main ::  IO ()
main = do
	putStrLn $ show(take 12 fibs)

