{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Char (digitToInt)
import Data.List (genericDrop)
import Control.Monad.State (State, execState, get, put, state)
import Control.Monad (liftM)
import Debug.Trace (trace)
import System.IO.Unsafe

newtype RowValue = RowValue Integer
    deriving (Num, Show, Ord, Eq, Enum)
newtype AlreadyAccumulated = AlreadyAccumulated Integer
    deriving (Num, Show, Ord, Eq)

data Row = Row
    String
    RowValue
    AlreadyAccumulated
    deriving (Show, Ord, Eq)

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

allRows :: [Row]
allRows = map (\(s,x) -> Row s x 0) $ zip allInfStrings [1..]

initialState = (allRows, 0)

--------------------------------------------------------------------------------

main :: IO ()
main = print $ snd $ execState compute initialState

traceThis :: (Show a) => String -> a -> State s a
traceThis msg x = state (\s -> trace ("trace: " ++ msg ++ " " ++ show x) (x,s))

compute :: State ([Row], Integer) ()
compute = do
    (rs, result) <- get
--    traceThis "initial state:" result
    compute' [1..80000]
--    compute' [1..2000000000000000]
--
    --    20000: 3 Sekunden
    --    30000: 6 Sekunden
    --    40000: 12 Sekunden (ohne prof: 4.2 Sekunden)
    --    80000: 50 Sekunden (ohne prof: 17.2 Sekunden)
    --    200000: 

compute' :: [Integer] -> State ([Row], Integer) ()
compute' runningNumbers = do
    mapM_ consolidate runningNumbers
    return ()

-- Compute p(k) for a given k. That is, search all rows for a matching sum.
consolidate :: Integer -> State ([Row], Integer) ()
consolidate k = do
    (rs, result) <- get
    consolidate' k [] rs
    (_, result1) <- get
--    traceThis "-----^ result of consolidate:" result1
    return ()

consolidate' :: Integer
             -> [Row] -- left side
             -> [Row] -- right side
             -> State ([Row], Integer) ()
consolidate' k left (cur:right)
    | fst (checkRow k cur) = do
--        if k `mod` 1000 == 0
--            then traceThis ("cons' with k: ") k
--            else return 0
        (rows, result) <- get
        put (left ++ [snd (checkRow k cur)] ++ right, result + rowNumber cur)
        (_, result1) <- get
--        traceThis ("cons' before: ") cur
--        traceThis ("cons' after: ") $ snd (checkRow k cur)
--        traceThis ("cons' found p(" ++ show k ++ "):") result1
        return ()
    | otherwise = consolidate' k (left ++ [cur]) right

checkRow :: Integer -> Row -> (Bool, Row)
checkRow k r@(Row seq rv (AlreadyAccumulated acc))  = case checkSequence seq k acc of
    Nothing -> (False, r)
    Just newSeq -> (True, Row newSeq rv (AlreadyAccumulated k))

checkSequence :: String
              -> Integer    -- k
              -> Integer    -- temp accumulator
              -> Maybe String  -- new Sequence
checkSequence (h:ts) k acc
    | h == '0' = checkSequence ts k leftValue
    | leftValue > k = Nothing
    | leftValue == k = Just ts
    | otherwise = checkSequence ts k leftValue
  where leftValue = case h of   -- much faster than acc +read [h]
            '0' -> acc
            '1' -> acc + 1
            '2' -> acc + 2
            '3' -> acc + 3
            '4' -> acc + 4
            '5' -> acc + 5
            '6' -> acc + 6
            '7' -> acc + 7
            '8' -> acc + 8
            '9' -> acc + 9

rowNumber :: Row -> Integer
rowNumber (Row _ (RowValue x) _) = x
