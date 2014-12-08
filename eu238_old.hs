{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Char (digitToInt)
import Data.List 
import Prelude hiding (drop)

newtype K = K Integer deriving (Show, Enum, Num, Eq)
newtype ValueConsumed = ValueConsumed Integer deriving (Show, Enum, Num, Eq, Ord)

data InfStr = InfStr String K ValueConsumed deriving (Show)
_infStr (InfStr a _ _) = a
_infK (InfStr _ b _) = b
_infVC (InfStr _ _ c) = c

s0 = 14025256

f n = n*n `mod` 20300713

str :: Integer -> String
str n = show n ++ str (f n)

infStr :: String
infStr = str s0

-- an infinite list of infinite strings
allInfStrings :: [String]
allInfStrings = allInfStrings' 0

allInfStrings' :: Integer -> [String]
allInfStrings' n = genericDrop n infStr : allInfStrings' (n+1)

allNumberedInfStrings :: [InfStr]
allNumberedInfStrings = map (\(x,y,z) -> InfStr x y z) $ zip3 allInfStrings [1..] $ repeat 0

--------------------------------------------------------------------------------

main = print $ allPKs 9 1 allNumberedInfStrings

bla x = allPKs x 1 allNumberedInfStrings

allPKs :: Integer   -- max k
       -> Integer   -- current k
       -> [InfStr]
       -> [K]
allPKs maxK curK _
    | maxK < curK = []
allPKs maxK curK strings = [k] ++ (allPKs maxK (curK + 1) newStrings)
    where (k, newStrings) = findK (curK) strings

-- findK 10 allNumberedInfStrings works fine; doesn't with allPKs
findK :: Integer    -- k
      -> [InfStr]
      -> (K, [InfStr])
findK k strings = findK' k strings []

findK' :: Integer
       -> [InfStr]
       -> [InfStr]
       -> (K, [InfStr])
findK' k (h:ts) old
    | isContainedInString = (_infK newString, old ++ [newString] ++ ts)
    | otherwise = findK' k ts (old ++ [newString])
        where 
            (isContainedInString, newString) = testForK k h (_infVC h)

testForK :: Integer        -- k
         -> InfStr         -- infinite string representation
         -> ValueConsumed  -- accumulator for speed (hopefully)
         -> (Bool, InfStr) -- actual result and new, shorter
                           -- string representation for speed
testForK k string acc
    | maybeK == fromIntegral k = {-# SCC "==case" #-} (True, newInfStr)
    | maybeK > fromIntegral k = {-# SCC ">case" #-} (False, string)
    | otherwise = {-# SCC "otherwiseCase" #-}
        testForK k constrStr maybeK 
        where
            infS = _infStr string
            tinfS = tail infS
            infK = _infK string
            constrStr = InfStr tinfS infK acc
            headCount = ValueConsumed ( fromIntegral $ digitToInt $ head $ infS) 
            maybeK = {-# SCC "maybeK" #-} 
                acc + headCount
            newInfStr = InfStr (tinfS) 
                               (infK) 
                               (_infVC string + headCount)



t01 k = testForK k string acc
    where 
          string = InfStr "140252567410149584700380" (K 1) (ValueConsumed 0)
          acc = ValueConsumed 0

-- should be 2, not 1
t02 = findK 4 strings
    where strings = [
            InfStr "40252567410149584700380" (K 1) (ValueConsumed 1),
            InfStr "40252567410149584700380" (K 2) (ValueConsumed 0),
            InfStr "52567410149584700380" (K 3) (ValueConsumed 2)
           ]

-- should be false
t03 = testForK 4  (InfStr "40252567410149584700380" (K 1) (ValueConsumed 1)) 0

-- 1 second for max k == 500
-- 7 seconds for max k == 1000
-- 54 seconds for max k == 2000
-- max k == 2*10^15 ?
-- new: 9 seconds for 10000
-- new: 39 seconds for 20000
-- 7 minutes for 100000 (10^5)
-- after refactoring: 2.2 seconds for 10000 O_o
-- after refactoring: 8.5 seconds for 20000
-- after refactoring: 3:40 minutes for 100000
-- more refactoring: 5.8 seconds for 10000 :(
-- more refactoring: 25.3 seconds for 20000 :(
-- further refactoring: 18.7 seconds for 10000




-- Untested

allPKs maxK strings = map fst
  [                                          findK 1 strings
  ,                            findK 2 (snd (findK 1 strings))
  ,              findK 3 (snd (findK 2 (snd (findK 1 strings))))
  ,findK 4 (snd (findK 3 (snd (findK 2 (snd (findK 1 strings))))))
  ]

allPKs maxK strings = (map (fst . fst) . take 4 . tail)
 (iterate (\((_,strings),n) -> (findK n strings,n + 1))
          ((undefined,strings),1))

allPKs maxK strings = (map (fst . fst) . tail)
  (unfoldr (\((_,strings),n) -> guard (n <= 4)
                             >> return (findK n strings,n + 1))
           ((undefined,strings),1))

allPKs maxK strings = (map fst . tail)
 (scanl (\(_,strings) n -> findK n strings) (undefined,strings) [1 .. 4])

allPKs maxK strings = (map fst . tail)
 (scanl (flip findK . snd) (undefined,strings) [1 .. 4])
