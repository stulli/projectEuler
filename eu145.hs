bla = [n | n <- [0..999999], (odd' (show (n + (rev n)))), (length $ show n) == (length $ show $ rev n)]

rev :: Int -> Int
rev n = read $ reverse $ show n

odd' :: String -> Bool
odd' [] = True
odd' n = if (odd $ read $ [head n]) then (odd' $ tail n) else False

huhu = length bla

enum n 
    | n `mod` 2 == 0 = let n' = n `div` 2
		       in 20 * 30 ^ (n' - 1)
    | otherwise = let n' = n `div` 2
		      n'' = n' `div` 2
		  in if even n'
		     then 0
		     else 5 * 20 * (20 * 25) ^ n''
result = sum $ map enum [2..9]
