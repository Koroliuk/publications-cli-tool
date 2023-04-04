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

execute (Command "update" (ptype:args)) context = case ptype of
    "Book"    -> updateBook args context
    "Article" -> updateArticle args context
    "Thesis"  -> updateThesis args context
    _         -> putStrLn "Invalid publication type"

execute (Command "getType" args) context
    | (length args) /= 1 = error "Invalid args"
    | otherwise = do
        publications <- readAllPublications (dbFilePath context)
        putStrLn $ show (getPublicationTypeByTitle (head args) publications)

execute (Command "getByAuthor" (ptype:args)) context = do 
    publications <- readAllPublications (dbFilePath context)
    case ptype of
        "Book"    -> putStrLn $ show (getPublicationByAuthorWithPredicate (head args) (\x -> case x of Book{} -> True; _ -> False) publications)
        "Article" -> putStrLn $ show (getPublicationByAuthorWithPredicate (head args) (\x -> case x of Article{} -> True; _ -> False) publications)
        "Thesis"  -> putStrLn $ show (getPublicationByAuthorWithPredicate (head args) (\x -> case x of Thesis{} -> True; _ -> False) publications)
        _         -> putStrLn $ "specify type"

execute (Command "getByAuthSingle" args) context
    | (length args) /= 1 = error "Invalid args"
    | otherwise = do
        publications <- readAllPublications (dbFilePath context)
        putStrLn $ show (getSingleAutoredPublicationsByAuthor (head args) publications)

execute (Command "publishers" []) context = do 
    publications <- readAllPublications (dbFilePath context)
    putStrLn $ show (listAllPublishers publications)
execute (Command "publishers" args) context = putStrLn "Invalid args"

execute (Command "journals" []) context = do 
    publications <- readAllPublications (dbFilePath context)
    putStrLn $ show (listAllJournals publications)
execute (Command "journals" args) context = putStrLn "Invalid args"

execute (Command "conferences" []) context = do 
    publications <- readAllPublications (dbFilePath context)
    putStrLn $ show (listAllConferences publications)
execute (Command "conferences" args) context = putStrLn "Invalid args"

execute (Command "statistics" []) context = do 
    publications <- readAllPublications (dbFilePath context)
    putStrLn $ show (getStatistics publications)
execute (Command "statistics" args) context = putStrLn "Invalid args"

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

updateBook :: [String] -> Context -> IO ()
updateBook [t, city, publisher, yearStr, authorsStr] context = do
    publications <- readAllPublications (dbFilePath context)
    let alreadySaved = [pub | pub <- publications, title pub == t]
    if (length alreadySaved > 0)
        then do 
            let newContent = [pub | pub <- publications, title pub /= t]
            let authors = words $ map (\c -> if c == ',' then ' ' else c) authorsStr
                year = read yearStr :: Int
                book = Book authors t city publisher year    
                tempfile = "temp.txt"
            withFile tempfile WriteMode $ \handle -> do
                forM_  (newContent ++ [book]) $ \pub -> do
                    hPutStrLn handle (show pub)
            removeFile (dbFilePath context)
            renameFile tempfile (dbFilePath context)
    else do
        putStrLn $ "Such book doesnt exist"
updateBook _ content = putStrLn "Invalid arguments for Book."

updateArticle :: [String] -> Context -> IO ()
updateArticle [t, journal, yearStr, issueStr, pagesStr, authorsStr] context = do
    publications <- readAllPublications (dbFilePath context)
    let alreadySaved = [pub | pub <- publications, title pub == t]
    if (length alreadySaved > 0)
        then do
            let newContent = [pub | pub <- publications, title pub /= t]
            let authors = words $ map (\c -> if c == ',' then ' ' else c) authorsStr
                year = read yearStr :: Int
                issue = read issueStr :: Int
                (startPage, endPage) = (read $ takeWhile (/= '-') pagesStr, read $ tail $ dropWhile (/= '-') pagesStr) :: (Int, Int)
                article = Article authors t journal year issue (startPage, endPage)
                tempfile = "temp.txt"
            withFile tempfile WriteMode $ \handle -> do
                forM_  (newContent ++ [article]) $ \pub -> do
                    hPutStrLn handle (show pub)
            removeFile (dbFilePath context)
            renameFile tempfile (dbFilePath context)
    else do
        putStrLn $ "Such artickle doesnt exist"
updateArticle _ content = putStrLn "Invalid arguments for Article."

updateThesis :: [String] -> Context -> IO ()
updateThesis [t, conference, city, yearStr, pagesStr, authorsStr] context = do
    publications <- readAllPublications (dbFilePath context)
    let alreadySaved = [pub | pub <- publications, title pub == t]
    if (length alreadySaved > 0)
        then do
            let newContent = [pub | pub <- publications, title pub /= t]
            let authors = words $ map (\c -> if c == ',' then ' ' else c) authorsStr
                year = read yearStr :: Int
                (startPage, endPage) = (read $ takeWhile (/= '-') pagesStr, read $ tail $ dropWhile (/= '-') pagesStr) :: (Int, Int)
                thesis = Thesis authors t conference city year (startPage, endPage)
                tempfile = "temp.txt"
            withFile tempfile WriteMode $ \handle -> do
                forM_  (newContent ++ [thesis]) $ \pub -> do
                    hPutStrLn handle (show pub)
            removeFile (dbFilePath context)
            renameFile tempfile (dbFilePath context)
    else do
        putStrLn $ "Such thesis doesnt exist"    
updateThesis _ content = putStrLn "Invalid arguments for Thesis."

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
