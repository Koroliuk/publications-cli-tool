module Main (main) where

import System.Environment   
import Parser
import Executor
import Logger
import Publication

main :: IO ()
main = do
    args <- getArgs
    let parsed = createContexAndGetCommands args
    let context = fst parsed
    let commands = snd parsed
    mapM_ (\x -> execute x context) commands
