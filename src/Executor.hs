module Executor where

import Parser
import Data.Maybe (catMaybes)
import Publication
import Logger
import System.IO
import Control.Monad (forM_)
import System.Directory

execute :: Command -> Context -> IO()
execute (Command "create" (ptype:args)) context = case ptype of
    "Book"    -> createBook args context
    "Article" -> createArticle args context
    "Thesis"  -> createThesis args context
    _         -> putStrLn "Invalid publication type"

execute (Command "read" args) context
    | (length args) /= 1 = error "Invalid args"
    | otherwise = do
        publications <- readAllPublications (dbFilePath context)
        let publication = [pub | pub <- publications, title pub == head args]
        putStrLn $ "Read: " ++ show publication

execute (Command "delete" args) context
    | (length args) /= 1 = error "Invalid args"
    | otherwise = deleteByTitle (head args) context

execute (Command "Help" []) context = 
    putStr("book-store-tool [DBNAME] [-c|--command COMMANDNAME [ARGS]]\n" ++
        "\t[-l|--log LOGNAME]\n" ++
        "\t[-s|--silent]\n" ++
        "\t[--html FILENAME]\n" ++
        "\t[-h|--help]\n")

deleteByTitle :: String -> Context -> IO ()
deleteByTitle titleToDelete context = do
    publications <- readAllPublications (dbFilePath context)
    let updatedPublications = [pub | pub <- publications, title pub /= titleToDelete]
    let tempfile = "temp.txt"
    withFile tempfile WriteMode $ \handle -> do
        forM_ updatedPublications $ \pub -> do
            hPutStrLn handle (show pub)
    removeFile (dbFilePath context)
    renameFile tempfile (dbFilePath context)

    putStrLn $ "Successfully deleted publication with title: " ++ titleToDelete

    
createBook :: [String] -> Context -> IO ()
createBook [t, city, publisher, yearStr, authorsStr] context = do
    publications <- readAllPublications (dbFilePath context)
    let alreadySaved = [pub | pub <- publications, title pub == t]
    if (length alreadySaved > 0)
        then do error "Wrong title"
    else do
        let authors = words $ map (\c -> if c == ',' then ' ' else c) authorsStr
            year = read yearStr :: Int
            book = Book authors t city publisher year
        putStrLn $ "Created Book: " ++ show book
        savePublication (dbFilePath context) book
createBook _ content = putStrLn "Invalid arguments for Book."

createArticle :: [String] -> Context -> IO ()
createArticle [t, journal, yearStr, issueStr, pagesStr, authorsStr] context = do
    publications <- readAllPublications (dbFilePath context)
    let alreadySaved = [pub | pub <- publications, title pub == t]
    if (length alreadySaved > 0)
        then do error "Wrong title"
    else do
        let authors = words $ map (\c -> if c == ',' then ' ' else c) authorsStr
            year = read yearStr :: Int
            issue = read issueStr :: Int
            (startPage, endPage) = (read $ takeWhile (/= '-') pagesStr, read $ tail $ dropWhile (/= '-') pagesStr) :: (Int, Int)
            article = Article authors t journal year issue (startPage, endPage)
        putStrLn $ "Created Article: " ++ show article
        savePublication (dbFilePath context) article
createArticle _ content = putStrLn "Invalid arguments for Article."

createThesis :: [String] -> Context -> IO ()
createThesis [t, conference, city, yearStr, pagesStr, authorsStr] context = do
    publications <- readAllPublications (dbFilePath context)
    let alreadySaved = [pub | pub <- publications, title pub == t]
    if (length alreadySaved > 0)
        then do error "Wrong title"
    else do
        let authors = words $ map (\c -> if c == ',' then ' ' else c) authorsStr
            year = read yearStr :: Int
            (startPage, endPage) = (read $ takeWhile (/= '-') pagesStr, read $ tail $ dropWhile (/= '-') pagesStr) :: (Int, Int)
            thesis = Thesis authors t conference city year (startPage, endPage)
        putStrLn $ "Created Thesis: " ++ show thesis
        savePublication (dbFilePath context) thesis
createThesis _ content = putStrLn "Invalid arguments for Thesis."



savePublication :: FilePath -> Publication -> IO ()
savePublication path pub = do
    let content = show pub
    appendFile path (content++"\n")

readAllPublications :: FilePath -> IO [Publication]
readAllPublications path = do
    content <- readFile path
    let linesContent = lines content
    let publications = catMaybes $ map readPublication linesContent
    return publications

readPublication :: String -> Maybe Publication
readPublication line =
    case reads line of
        [(publication, "")] -> Just publication
        _                   -> Nothing
