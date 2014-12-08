import Data.Char (digitToInt)

f = sum . map digitToInt . show $ product [1..100]
