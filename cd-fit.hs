module Main where

import Text.ParserCombinators.Parsec
import Data.List(sortBy)

--parses output of "du -subdirectory"
parseInput = 
    do dirs <- many dirAndSize
       eof :: Parser ()
       return dirs

--datatype Dir holds information about a single directory
-- its size and name 
data Dir = Dir {dir_size::Int, dir_name::String} deriving Show

-- `dirAndSize` parses information about single directory, which is:
-- a size in bytes (number), some spaces, then directory name, which extends till newline
dirAndSize = 
    do size <- many1 digit
       spaces
       dir_name <- anyChar `manyTill` newline
       return (Dir (read size) dir_name)

--import Data.List(sortBy)

--DirPack holds a set of directories which are to be stored on a CD
--'pack_size' is stored separately
data DirPack = DirPack {pack_size::Int, dirs::[Dir]} deriving Show

--for simplicity, assume 700mb CD's
media_size = 700*1024*1024

--Greedy packer tries to add directories one by one to the initially empty 'DirPack'
greedy_pack dirs = foldl maybe_add_dir (DirPack 0 []) $ sortBy cmpSize dirs
    where
    cmpSize d1 d2 = compare (dir_size d1) (dir_size d2)

--helper function to add directory "d" to the pack "p" when
--new size doesn't exceed media_size
maybe_add_dir p d = 
    let new_size = pack_size p + dir_size d
        new_dirs = d:(dirs p)
        in if new_size > media_size then p else DirPack new_size new_dirs



main = do input <- getContents
          putStrLn ("DEBUG: got input " ++ input)
          --compute solution and print it 
          let dirs = case parse parseInput "stdin" input of
                          Left err -> error $ "Input:\n" ++ show input ++
                                              "\nError:\n" ++ show err
                          Right result -> result
          putStrLn "DEBUG: parsed:"; print dirs
          putStrLn "Solution:" ; print (greedy_pack dirs)
