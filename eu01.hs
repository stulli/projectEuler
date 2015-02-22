sumofmultiples :: Int -> Int -> Int-> Int
sumofmultiples m1 m2 i = sum $ getlistofmultiples m1 m2 i

getlistofmultiples :: Int -> Int-> Int -> [Int]
getlistofmultiples m1 m2 i = filter (/= 0) $ map f [1..i]
    where 
     cond = \x -> ((mod x m1) == 0) || ((mod x m2) == 0)
     f = \x -> if (cond x) then x else 0

main :: IO ()
main = print $ sumofmultiples 3 5 999
