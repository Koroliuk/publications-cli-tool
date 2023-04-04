module Main (main) where

import System.Environment   
import Parser
import Executor
import Logger

main :: IO ()
main = do
    args <- getArgs
    -- putStrLn $ show args
    let parsed = createContexAndGetCommands args
    let context = fst parsed
    let commands = snd parsed
    mapM_  (\x -> execute x context) commands
    -- putStrLn $ show parsed

    logMessage "Test message to log" context
