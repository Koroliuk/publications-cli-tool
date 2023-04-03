module Executor (
    execute
) where

import Parser

execute :: Command -> Context -> IO()
execute (Command "Help" []) context = 
    putStr("book-store-tool [DBNAME] [-c|--command COMMANDNAME [ARGS]]\n" ++
        "\t[-l|--log LOGNAME]\n" ++
        "\t[-s|--silent]\n" ++
        "\t[--html FILENAME]\n" ++
        "\t[-h|--help]\n")