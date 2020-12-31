import qualified Control.Monad as M
import qualified Data.List as L

-- Solution Algorithms

-- Compute the trace of a matrix, represented as a list of lists.
getTrace :: [[Int]] -> Int
getTrace mat = sum $ zipWith (!!) mat [0..nrows-1]
    where nrows = length mat

-- Check if a list contains only unique elements.
isUnique :: Eq a => [a] -> Int
isUnique lst = if (length $ L.nub lst) == (length lst) then 0 else 1

-- Check if matrix rows are unique.
nonUniqueRowsCount :: [[Int]] -> Int
nonUniqueRowsCount mat = sum $ map isUnique mat

-- Check if matrix columns are unique
nonUniqueColsCount :: [[Int]] -> Int
nonUniqueColsCount mat = nonUniqueRowsCount (L.transpose mat)

-- Check if matrix has unique rows and cols.
isLatin :: [[Int]] -> Bool
isLatin mat = (nonUniqueColsCount mat) + (nonUniqueRowsCount mat) > 0

-- IO Section
readInt :: IO Int
readInt = do
    x <- getLine
    return (read x)

readIntList :: IO [Int]
readIntList = do
    istrs <- getLine
    return (map read $ words istrs)


processCase :: Int -> IO ()
processCase caseNum = do
    nlines <- readInt
    mat <- M.replicateM nlines readIntList
    putStrLn $ "Case #" ++ (show caseNum) ++ ": " ++ (show $ getTrace mat) ++ " " ++ (show $ nonUniqueRowsCount mat) ++ " " ++ (show $ nonUniqueColsCount mat)

-- Entry point.
main :: IO ()
main = do
    t <- readInt
    M.forM_ [1..t] processCase