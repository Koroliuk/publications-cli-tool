module Executor where

import Parser
import Publication
import Logger
import qualified Data.ByteString.Char8 as BS (readFile, writeFile, unlines, unpack, pack, append)
import Control.Monad (forM_)
import System.Directory
import DBManager

execute :: Command -> Context -> IO()
execute (Command "create" (ptype:args)) context = case ptype of
    "Book"    -> createBook args context
    "Article" -> createArticle args context
    "Thesis"  -> createThesis args context
    _         -> putStrLn "Invalid publication type"

execute (Command "read" args@(targetTitle:_)) context
    | (length args) /= 1 = error "Зайві аргументи для читання"
    | otherwise = do
        publications <- readAllPublications (dbFilePath context)
        let publication = [pub | pub <- publications, title pub == targetTitle]
        logMessage ("Було зчитано:" ++ show publication) context

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
        logMessage ("Тип публікації: " ++ show (getPublicationTypeByTitle (head args) publications)) context

execute (Command "getByAuthor" (ptype:args)) context = do 
    publications <- readAllPublications (dbFilePath context)
    case ptype of
        "Book"    -> logMessage ("Автор книг:" ++ show (getPublicationByAuthorWithPredicate (head args) (\x -> case x of Book{} -> True; _ -> False) publications)) context
        "Article" -> logMessage ("Автор статей:" ++ show (getPublicationByAuthorWithPredicate (head args) (\x -> case x of Article{} -> True; _ -> False) publications)) context
        "Thesis"  -> logMessage ("Автор тез:" ++ show (getPublicationByAuthorWithPredicate (head args) (\x -> case x of Thesis{} -> True; _ -> False) publications)) context
        _         -> putStrLn $ "specify type"

execute (Command "getByAuthSingle" args) context
    | (length args) /= 1 = error "Invalid args"
    | otherwise = do
        publications <- readAllPublications (dbFilePath context)
        logMessage ("Єдиний автор для: " ++ show (getSingleAutoredPublicationsByAuthor (head args) publications)) context

execute (Command "publishers" []) context = do 
    publications <- readAllPublications (dbFilePath context)
    logMessage ("Видавництва: " ++ show (listAllPublishers publications)) context
execute (Command "publishers" args) context = putStrLn "Invalid args"

execute (Command "journals" []) context = do 
    publications <- readAllPublications (dbFilePath context)
    logMessage ("Журнали: " ++ show (listAllJournals publications)) context
execute (Command "journals" args) context = putStrLn "Invalid args"

execute (Command "conferences" []) context = do 
    publications <- readAllPublications (dbFilePath context)
    logMessage ("Конференції: " ++ show (listAllConferences publications)) context
execute (Command "conferences" args) context = putStrLn "Invalid args"

execute (Command "statistics" []) context = do 
    publications <- readAllPublications (dbFilePath context)
    logMessage ("Статистика: " ++ show (getStatistics publications)) context
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
    let updatedPublications = [show pub | pub <- publications, title pub /= titleToDelete]
    let updatedPublicationsString = foldl (\acc pub -> acc ++ (show pub) ++ "\n") "" updatedPublications
    BS.writeFile (dbFilePath context) $ BS.pack updatedPublicationsString 
    logMessage ("Successfully deleted publication with title: " ++ titleToDelete) context

    
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
        logMessage ("Created Book: " ++ show book) context
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
        logMessage ("Created Article: " ++ show article) context
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
        logMessage ("Created Thesis: " ++ show thesis) context
        savePublication (dbFilePath context) thesis
createThesis _ content = putStrLn "Invalid arguments for Thesis."

updateBook :: [String] -> Context -> IO ()
updateBook [t, city, publisher, yearStr, authorsStr] context = do
    publications <- readAllPublications (dbFilePath context)
    let alreadySaved = [pub | pub <- publications, title pub == t]
    putStrLn $ show (publications)
    if (length alreadySaved > 0)
        then do 
            let updatedPublications = [pub | pub <- publications, title pub /= t]
            let authors = words $ map (\c -> if c == ',' then ' ' else c) authorsStr
                year = read yearStr :: Int
                book = Book authors t city publisher year    
            let updatedPublicationsString = foldl (\acc pub -> acc ++ (show pub) ++ "\n") "" (updatedPublications++[book])
            BS.writeFile (dbFilePath context) $ BS.pack updatedPublicationsString 
            logMessage ("Updated Book with title: " ++ t) context
    else do
        putStrLn $ "Such book doesnt exist"
updateBook _ content = putStrLn "Invalid arguments for Book."

updateArticle :: [String] -> Context -> IO ()
updateArticle [t, journal, yearStr, issueStr, pagesStr, authorsStr] context = do
    publications <- readAllPublications (dbFilePath context)
    let alreadySaved = [pub | pub <- publications, title pub == t]
    if (length alreadySaved > 0)
        then do
            let updatedPublications = [pub | pub <- publications, title pub /= t]
            let authors = words $ map (\c -> if c == ',' then ' ' else c) authorsStr
                year = read yearStr :: Int
                issue = read issueStr :: Int
                (startPage, endPage) = (read $ takeWhile (/= '-') pagesStr, read $ tail $ dropWhile (/= '-') pagesStr) :: (Int, Int)
                article = Article authors t journal year issue (startPage, endPage)
            let updatedPublicationsString = foldl (\acc pub -> acc ++ (show pub) ++ "\n") "" (updatedPublications++[article])
            BS.writeFile (dbFilePath context) $ BS.pack updatedPublicationsString 
            logMessage ("Updated Article with title: " ++ t) context
    else do
        putStrLn $ "Such artickle doesnt exist"
updateArticle _ content = putStrLn "Invalid arguments for Article."

updateThesis :: [String] -> Context -> IO ()
updateThesis [t, conference, city, yearStr, pagesStr, authorsStr] context = do
    publications <- readAllPublications (dbFilePath context)
    let alreadySaved = [pub | pub <- publications, title pub == t]
    if (length alreadySaved > 0)
        then do
            let updatedPublications = [pub | pub <- publications, title pub /= t]
            let authors = words $ map (\c -> if c == ',' then ' ' else c) authorsStr
                year = read yearStr :: Int
                (startPage, endPage) = (read $ takeWhile (/= '-') pagesStr, read $ tail $ dropWhile (/= '-') pagesStr) :: (Int, Int)
                thesis = Thesis authors t conference city year (startPage, endPage)
            let updatedPublicationsString = foldl (\acc pub -> acc ++ (show pub) ++ "\n") "" (updatedPublications++[thesis])
            BS.writeFile (dbFilePath context) $ BS.pack updatedPublicationsString 
            logMessage ("Updated Thesis with title: " ++ t) context
    else do
        putStrLn $ "Such thesis doesnt exist"    
updateThesis _ content = putStrLn "Invalid arguments for Thesis."

