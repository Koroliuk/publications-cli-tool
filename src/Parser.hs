module Parser (
    createContexAndGetCommands,
    Command,
    Context
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


createContexAndGetCommands :: [String] -> (Context, [Command])
createContexAndGetCommands argsList = 
    let parsed = parse(argsList)
    in ((buildContex parsed), (getCommands (snd(parsed))))

getCommands :: [(String, [String])] -> [Command]
getCommands [] = []
getCommands (x@(flag, args):xs)
    | (fst(x) == "-h" || fst(x) == "--help") =  (getCommands xs) ++ [Command "Help" []]
    | (fst(x) == "-c" || fst(x) == "--command") = (getCommands xs) ++ [Command (head args) (tail args)]
    | otherwise = getCommands xs

buildContex :: (String, [(String, [String])]) -> Context
buildContex (dbName, arrList) = 
    let htmlPair = findByFlagNames arrList ["--html"]
        silent = findByFlagNames arrList ["-s", "--silent"]
        fileLog = findByFlagNames arrList ["-l", "--log"]
    in Context {
        dbFilePath = dbName,
        logToHTML = htmlPair /= ("", []),
        htmlFilePath = if (htmlPair /= ("", [])) then head(snd(htmlPair)) else "",
        logToConsole = not (silent /= ("", [])),
        logToFile = fileLog /= ("", []),
        logFilePath = if (fileLog /= ("", [])) then head(snd(fileLog)) else ""
    }


findByFlagNames :: [(String, [String])] -> [String] -> (String, [String])
findByFlagNames [] flagNames = ("", [])
findByFlagNames (x:xs) flagNames
    | fst(x) `elem` flagNames = x
    | otherwise = findByFlagNames xs flagNames

parse :: [String] -> (String, [(String, [String])])
parse [] = error "database file path must be provided"
parse (x:xs) = (x, args) where
    args = validateArgs(extractArgs xs)

extractArgs :: [String] -> [(String, [String])]
extractArgs argsList =
    foldl (\acc arg -> addArg arg acc) [] argsList where
        addArg argI result
            | (not (isAllowedFlag argI)) && result == [] = error "wrong argument, please for optional behavior use flags"
            | (isAllowedFlag argI) = result ++ [(argI, [])]
            | otherwise = init result ++ [(fst (last result), snd (last result) ++ [argI])]

allowedFlasg :: [String]
allowedFlasg = ["-c", "--command", "-l", "--log", "-s", "--silent", "--html", "-h", "--help"]

isAllowedFlag :: String -> Bool
isAllowedFlag s = s `elem` allowedFlasg

allowedCommand :: [String]
allowedCommand = ["create"]

validateArgs :: [(String, [String])] -> [(String, [String])]
validateArgs argsList = 
    let mentionedflags = foldl (\acc flagAndArgs -> acc ++ [fst(flagAndArgs)]) [] argsList
    in filter (\(flag, args) -> check flag args mentionedflags) argsList where 
        check flag argss flags 
            | flag == "-c" && "--command" `elem` flags = error "use only -c or --command not both"
            | (flag == "-c" || flag == "--command") && head(argss) `notElem` allowedCommand = error "wrong command provided"
            | flag == "-l" && "--log" `elem` flags = error "use only -l or --log not both"
            | (flag == "-l" || flag == "--log") && length argss /= 1 = error "no file or more that one provided"
            | flag == "-s" && "--silent" `elem` flags = error "use only -s or --silent not both"
            | (flag == "-s" || flag == "--silent") && length argss /= 0 = error "no args must be provided for -s/--silent"
            | flag == "--html" && length argss /= 1 = error "no file or more that one provided"
            | flag == "-h" && "--help" `elem` flags = error "use only -h or --help not both"
            | (flag == "-h" || flag == "--help") && length argss /= 0 = error "no args must be provided for -h/--help"
            | otherwise = True
