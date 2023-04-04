module Main (main) where

import System.Environment   
import Parser
import Executor
import Logger
import Publication

main :: IO ()
main = do
    args <- getArgs
    -- putStrLn $ show args
    let parsed = createContexAndGetCommands args
    let context = fst parsed
    let commands = snd parsed
    mapM_ (\x -> execute x context) commands
    -- putStrLn $ show parsed

    -- logMessage "Test message to log" context
    -- let book = Book ["John Doe", "Jane Smith"] "The Book Title" "New York" "Publisher Inc" 2023
    -- savePublication "publications.txt" book
    -- putStrLn "Publication saved to file."
    -- publications <- readAllPublications "publications.txt"
    -- putStrLn "Saved publications:"
    -- mapM_ print publications

-- $ my_program Book "The Title" "New York" "Publisher Inc" "2023" "John Doe, Jane Smith"
