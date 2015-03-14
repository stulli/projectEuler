import Data.List (permutations)

main = do
    let result = filter f $ permutations "0123456789"
    print $ sum $ map read result
    where
      f x = f1 x && f2 x && f3 x && f4 x && f5 x && f6 x && f7 x
      f1 x = read (x!!1 : x!!2 : [x!!3]) `mod` 2 == 0
      f2 x = read (x!!2 : x!!3 : [x!!4]) `mod` 3 == 0
      f3 x = read (x!!3 : x!!4 : [x!!5]) `mod` 5 == 0
      f4 x = read (x!!4 : x!!5 : [x!!6]) `mod` 7 == 0
      f5 x = read (x!!5 : x!!6 : [x!!7]) `mod` 11 == 0
      f6 x = read (x!!6 : x!!7 : [x!!8]) `mod` 13 == 0
      f7 x = read (x!!7 : x!!8 : [x!!9]) `mod` 17 == 0
