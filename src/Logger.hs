module Logger (
    logMessage
) where

import Control.Monad (when)
import System.Directory (doesFileExist)
import Parser
import System.IO

start = "<!DOCTYPE html>\n<html>\n<body>\n"
end = "</body>\n</html>"

logMessage :: String -> Context -> IO()
logMessage message context = do
    if (logToConsole context) 
        then putStrLn $ message
    else return ()

    if (logToFile context) 
        then appendFile (logFilePath context) (message++"\n")
    else return ()

    if (logToHTML context) 
        then do
            isFileAlreadyExists <- doesFileExist (htmlFilePath context)
            if (not isFileAlreadyExists) 
                then appendFile (htmlFilePath context) (start ++ "<p>" ++ message ++ "</p>\n" ++ end)
                else do
                    content <- readFile (htmlFilePath context)
                    let linesContent = lines content
                    let nonEmptyLines = filter (not . null) linesContent
                    let nonEmptyLinesToRemove = 2

                    if length nonEmptyLines <= nonEmptyLinesToRemove
                        then error "There are not enough non-empty lines to remove."
                    else do
                        let nonEmptyLinesUpdated = take (length nonEmptyLines - nonEmptyLinesToRemove) nonEmptyLines
                        let updatedContent = (unlines nonEmptyLinesUpdated) ++ "<p>" ++ message ++ "</p>\n" ++ end

                        withFile (htmlFilePath context) WriteMode $ \handle ->
                            hPutStr handle updatedContent
    else return ()
