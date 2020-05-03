module Main where
c = putStrLn "C" 
main = do putStrLn "hello"
          putStrLn "goodbye"
          putStrLn "hello again"
          do combine c c
             let b = combine (putStrLn "Hey there") (putStrLn "Byebye")
             let d = combine (b) (combine c c)
             putStrLn "buhbye!"

--{ putStrLn "hello"; putStrLn "goodbye";};

combine before after = 
    do before
       putStrLn "in the middle"
       after
       

