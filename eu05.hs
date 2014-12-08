lcm' :: Int -> Int
lcm' 2 = 2
lcm' n = lcm n (lcm' (n-1))

-- foldr (lcm) 1 [1..20]
