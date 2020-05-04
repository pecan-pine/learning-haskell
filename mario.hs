module Main where

import Data.List

--draw :: Int -> IO ()
--draw top row as nothing
draw 0 n = putStr ""
--draw rows (n will always be the height)
draw i n = do
    draw (i-1) n
    rstring (n-i) " "
    rstring i "#"
    putStr "  "
    rstringLn i "#"

--print string s repeated n times
rstring n s = putStr $ intercalate "" $ replicate n s

rstringLn n s = putStrLn $ intercalate "" $ replicate n s

main = do
    putStrLn "Height:"
    input <- getLine
    let height = read input :: Int
    draw height height
