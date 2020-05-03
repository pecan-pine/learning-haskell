module Main where

import Text.ParserCombinators.Parsec

--parses output of "du -subdirectory"
parseInput = 
    do dirs <- many dirAndSize
       eof :: Parser ()
       return dirs

--datatype Dir holds information about a single directory
-- its size and name 
data Dir = Dir Int String deriving Show

-- `dirAndSize` parses information about single directory, which is:
-- a size in bytes (number), some spaces, then directory name, which extends till newline
dirAndSize = 
    do size <- many1 digit
       spaces
       dir_name <- anyChar `manyTill` newline
       return (Dir (read size) dir_name)


main = do input <- getContents
          putStrLn ("DEBUG: got input " ++ input)
          --compute solution and print it 
          let dirs = case parse parseInput "stdin" input of
                          Left err -> error $ "Input:\n" ++ show input ++
                                              "\nError:\n" ++ show err
                          Right result -> result
          putStrLn "DEBUG: parsed:"; print dirs
