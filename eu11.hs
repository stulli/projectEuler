import Data.List
import System.IO
import Data.Char

main = do
    content <- readFile "grid"
    let intGrid = map (map read . words) $ lines content::[[Int]]
    let resultHor = maximum $ map prodHorLine intGrid
    let resultVer = maximum $ map prodHorLine $ transpose intGrid
    let resultDia1 = dia intGrid
    let resultDia2 = dia $ map reverse intGrid
    putStrLn $ show $ [resultHor, resultVer, resultDia1, resultDia2]

prodHori grid = maximum $ map prodHorLine grid

prodHorLine = maximum . prodsHorLine 

prodsHorLine :: [Int] -> [Int]
prodsHorLine (a:b:c:[]) = []
prodsHorLine (a:b:c:d:xs) = (a*b*c*d) : (prodsHorLine (b:c:d:xs))

dia = maximum . dias'

dias' (_:_:_:[]) = []
dias' g = (maximum $ dias g) : (dias' $ tail g)

dias (_:_:_:(d1:d2:d3:[]):_) = []
dias (_:_:_:[]) = []
dias ((a1:ar):(b1:b2:br):(c1:c2:c3:cr):(d1:d2:d3:d4:dr):r) = 
    (a1*b2*c3*d4) : (dias (ar:(b2:br):(c2:c3:cr):(d2:d3:d4:dr):r))

