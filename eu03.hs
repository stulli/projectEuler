prime_factors_of 1 = []
prime_factors_of n = f : prime_factors_of(n `div` f)
    where f = head $ filter (\d -> n `mod` d == 0) [2..n]
