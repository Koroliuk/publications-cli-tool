module Parser (
    parse
) where

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
