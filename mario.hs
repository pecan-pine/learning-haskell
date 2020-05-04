module Main where

import Data.List

--draw :: Int -> IO ()
draw 1 = putStrLn "#"
draw n = do
    draw(n-1)
    putStrLn $ intercalate "" $ replicate n "#"
    

main = do
    putStrLn "Height:"
    input <- getLine
    let height = read input :: Int
    draw height 
