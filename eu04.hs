is_pal :: Int -> Bool
is_pal n = reverse (show n) == show n

pal2 = maximum (filter is_pal [a*b | a <- [100..999], b <- [a..999]])

main :: IO ()
main = print pal2
