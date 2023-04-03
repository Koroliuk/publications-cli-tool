module Main (main) where

import System.Environment   
import Parser

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ show args
    let parsed = createContexAndGetCommands args
    putStrLn $ show parsed
