module Main where

import Data.List (nub, sort)

main = print $ sum $ nub $ g

g = [ a*b | 
        a <- [1..50::Integer], 
        b <- [a..2000], 
        checkLength a b]
    where 
      cons a b= 
        show a ++
        show b ++ 
        show (a * b )
      checkLength a b  = 
        length (cons a b ) == 9 && 
        '0' `notElem` cons a b &&
        sort (cons a b ) == "123456789"
