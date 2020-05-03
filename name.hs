module Main where

main = do 
    putStrLn "What is your name?"
    name <- getLine
    putStrLn ("Hello, " ++ name ++ " how are you?")
    putStrLn ("What is your favorite color?")
    color <- getLine
    putStrLn ("Your favorite color is " ++ color ++ "!")
    
    

{--input <- getContents
          putStrLn ("DEBUG: got input " ++ input)
          --compute solution and print it --}
