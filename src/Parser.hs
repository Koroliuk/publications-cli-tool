module Parser (
    Command(..), 
    Context(..),
    createContexAndGetCommands
) where

data Command = 
    Command {
        name::String,
        args::[String]
    } deriving(Show)

data Context =
    Context {
        dbFilePath::String,
        logToHTML::Bool,
        htmlFilePath::String,
        logToConsole::Bool,
        logToFile::Bool,
        logFilePath::String
    } deriving(Show)

allowedFlasg :: [String]
allowedFlasg = ["-c", "--command", "-l", "--log", "-s", "--silent", "--html", "-h", "--help"]

allowedCommand :: [String]
allowedCommand = ["create", "read", "update", "delete", "getType", "getByAuthor", "getByAuthSingle", 
    "publishers", "journals", "conferences", "statistics"]

createContexAndGetCommands :: [String] -> (Context, [Command])
createContexAndGetCommands argsList = 
    let flagArgsMap = parse(argsList)
    in (buildContex flagArgsMap, getCommands $ snd flagArgsMap) where

getCommands :: [(String, [String])] -> [Command]
getCommands [] = []
getCommands ((flag, argsList):xs)
    | (flag == "-h" || flag == "--help") =  (getCommands xs) ++ [Command "Help" []]
    | (flag == "-c" || flag == "--command") = (getCommands xs) ++ [Command (head argsList) (tail argsList)]
    | otherwise = getCommands xs

buildContex :: (String, [(String, [String])]) -> Context
buildContex (dbName, argsList) = 
    let htmlPair = findByFlagNames argsList ["--html"]
        silent = findByFlagNames argsList ["-s", "--silent"]
        fileLog = findByFlagNames argsList ["-l", "--log"]
    in Context {
        dbFilePath = dbName,
        logToHTML = htmlPair /= ("", []),
        htmlFilePath = if (htmlPair /= ("", [])) then head $ snd htmlPair else "",
        logToConsole = not (silent /= ("", [])),
        logToFile = fileLog /= ("", []),
        logFilePath = if (fileLog /= ("", [])) then head $ snd fileLog  else ""
    }

findByFlagNames :: [(String, [String])] -> [String] -> (String, [String])
findByFlagNames [] _ = ("", [])
findByFlagNames (x:xs) flagNames
    | (fst x) `elem` flagNames = x
    | otherwise = findByFlagNames xs flagNames

parse :: [String] -> (String, [(String, [String])])
parse [] = error "файл бази даних має бути вказаним"
parse ["-h"] = ("", [("-h", [])])
parse ["--help"] = ("", [("--help", [])])
parse (x:xs) = (x, argsList) where
    argsList = validateArgs $ extractArgs xs

extractArgs :: [String] -> [(String, [String])]
extractArgs argsList =
    foldl (\acc arg -> addArg arg acc) [] argsList where
        addArg argI result
            | (not (isAllowedFlag argI)) && result == [] = error "зайвий аргумент, для опціональної поведінки викоистовуйте прапорці"
            | (isAllowedFlag argI) = result ++ [(argI, [])]
            | otherwise = init result ++ [(fst (last result), snd (last result) ++ [argI])]

isAllowedFlag :: String -> Bool
isAllowedFlag s = s `elem` allowedFlasg

validateArgs :: [(String, [String])] -> [(String, [String])]
validateArgs flagArgsList = 
    let mentionedflags = foldl (\acc flagAndArgs -> acc ++ [fst flagAndArgs]) [] flagArgsList
    in filter (\(flag, argsList) -> check flag argsList mentionedflags) flagArgsList where 
        check flag argsList flags 
            | flag == "-c" && "--command" `elem` flags = error "використовуйте або -c, або --command"
            | (flag == "-c" || flag == "--command") && (head argsList) `notElem` allowedCommand = error "вказана неправилна команда"
            | flag == "-l" && "--log" `elem` flags = error "використовуйте або -l, або --log"
            | (flag == "-l" || flag == "--log") && length argsList /= 1 = error "файл логу не вказани або вказано кілька"
            | flag == "-s" && "--silent" `elem` flags = error "використовуйте або -s, або --silent"
            | (flag == "-s" || flag == "--silent") && length argsList /= 0 = error "зайві аргументи для -s/--silent"
            | flag == "--html" && length argsList /= 1 = error "html файл не вказани або вказано кілька"
            | flag == "-h" && "--help" `elem` flags = error "використовуйте або -h, або --hep"
            | (flag == "-h" || flag == "--help") && length argsList /= 0 = error "зайві аргументи для -h/--help"
            | otherwise = True
