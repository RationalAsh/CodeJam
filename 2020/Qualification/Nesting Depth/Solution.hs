import qualified Control.Monad as M

-- Solution algorithm
solveLevel :: Int -> [Char] -> [Char]
solveLevel lvl (c:chars)
    | cnum > lvl = (replicate (cnum-lvl) '(') ++ [c] ++ (solveLevel cnum chars)
    | cnum == lvl = [c] ++ (solveLevel cnum chars)
    | cnum < lvl =  (replicate (lvl-cnum) ')') ++ [c] ++ (solveLevel cnum chars)
    where cnum = (read [c]) :: Int

solveLevel lvl [] = replicate lvl ')'

-- IO Section
readInt :: IO Int
readInt = do
    x <- getLine
    return (read x)

processCase :: Int -> IO ()
processCase caseNum = do
    caseStr <- getLine
    putStrLn $ "Case #" ++ (show caseNum) ++ ": " ++ (solveLevel 0 caseStr)

-- Entry point.
main :: IO ()
main = do
    t <- readInt
    M.forM_ [1..t] processCase