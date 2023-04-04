module Publication where

data Publication = 
    Book {
        authors :: [String],
        title :: String,
        city :: String, 
        publisher :: String, 
        year :: Int
    } 
    | Article {
        authors :: [String],
        title :: String, 
        journal :: String, 
        year :: Int, 
        issue :: Int, 
        pages :: (Int, Int)
    } 
    | Thesis {
        authors :: [String], 
        title :: String, 
        conference :: String, 
        city :: String, 
        year :: Int, 
        pages :: (Int, Int)
    }
    deriving (Eq, Show, Read)  

