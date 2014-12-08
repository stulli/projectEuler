main = print $ sum $ take 2001 allNumbers

vierer s i = [s,s+i,s+2*i,s+3*i]

allNumbers = concat $ map (\(x,y) -> vierer x y) $ zip (map (^2) [1,3..]) [2,4..]
