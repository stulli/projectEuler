
f = product [n/d | n <- [10..99], d <- [10..99],
        n/d == stupid (show n) (show d), n /= d ]

stupid :: String -> String -> Rational
stupid (n1:n2:_) (d1:d2:_) 
    | n1 == n2 = 88888
    | d1 == d2 = 77777
    | d2 /= '0' && (d2 == n1 || d1 == n2) = read $ [n1] ++ "%" ++ [d2]
    | otherwise = 999999
