module DBManager where

import Publication
import Data.Maybe (catMaybes)
import qualified Data.ByteString.Char8 as BS (readFile, writeFile, unlines, unpack, pack, append)

savePublication :: FilePath -> Publication -> IO ()
savePublication path pub = do
    let publication = show pub
    contents <- BS.readFile path
    BS.writeFile path $ BS.append contents $ BS.pack (publication++"\n")

readAllPublications :: FilePath -> IO [Publication]
readAllPublications path = do
    content <- BS.readFile path
    let linesContent = lines  $ BS.unpack content
    let publications = catMaybes $ map readPublication linesContent
    return publications

readPublication :: String -> Maybe Publication
readPublication line =
    case reads line of
        [(publication, "")] -> Just publication
        _                   -> Nothing
