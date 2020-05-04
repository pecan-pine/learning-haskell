module Main where

import Data.List

--draw :: Int -> IO ()
--draw top row as nothing
draw 0 n = putStr ""
--draw top rows (n will always be the height)
draw i n = do
    draw (i-1) n
    putStr $ intercalate "" $ replicate (n-i) " "
    putStrLn $ intercalate "" $ replicate i "#"
    

main = do
    putStrLn "Height:"
    input <- getLine
    let height = read input :: Int
    draw height height
