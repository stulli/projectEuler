{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Char (digitToInt)
import Data.List (genericDrop)
import Control.Monad.Trans.State (State, execState, get, put)
import Debug.Trace (trace)

newtype RowValue = RowValue Integer
    deriving (Num, Show, Ord, Eq, Enum)
newtype AlreadyAccumulated = AlreadyAccumulated Integer
    deriving (Num, Show, Ord, Eq)
newtype Sequence = Sequence String
    deriving (Ord, Eq)

data Row = Row
    Sequence
    RowValue
    AlreadyAccumulated
    deriving (Show, Ord, Eq)

instance Show Sequence where
    show (Sequence s) = "(Sequence " ++ take 10 s ++ "...)"

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
allRows = map (\(s,x) -> Row (Sequence s) x 0) $ zip allInfStrings [1..]

initialState = (allRows, 0)

--------------------------------------------------------------------------------

main :: IO ()
main = do
    print $ take 5 $ fst $ execState compute initialState

compute :: State ([Row], Integer) ()
compute = do
    (rs, result) <- get
    case trySingleRow (head rs) 1 of
         Just (r,x) -> put (r : tail rs, result + x)
         Nothing -> return ()
    
    (rs, result) <- get
    trace ("result: " ++ show result) $ return ()

computeNewRows :: [Row] -- left side; empty list in the beginning
               -> [Row] -- right side; holds all rows at first
               -> Integer   -- current k
               -> [Row] -- new, updated rows
computeNewRows left right k = undefined

trySingleRow :: Row
             -> Integer     -- current k
             -> Maybe (Row, Integer)    -- new row and RowValue (this should be
                                        -- added to the final result)
trySingleRow r@(Row (Sequence s) (RowValue v) (AlreadyAccumulated a)) k =
    case getNewString s k of
        Nothing -> Nothing
        Just newS -> Just (Row (Sequence newS)
                               (RowValue v)
                               (AlreadyAccumulated (a+k)), v)

getNewString :: String -> Integer -> Maybe String
getNewString s k
    | digitToInt (head s) == fromIntegral k = Just (tail s)
    | digitToInt (head s) < fromIntegral k = getNewString (tail s) k
    | digitToInt (head s) > fromIntegral k = Nothing
