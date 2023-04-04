module Publication (
    Publication(..),
    getPublicationTypeByTitle, 
    getPublicationByAuthorWithPredicate,
    getSingleAutoredPublicationsByAuthor,
    listAllPublishers,
    listAllJournals,
    listAllConferences,
    getStatistics
) where

data Publication = 
    Book {
        authors::[String],
        title::String,
        city::String, 
        publisher::String, 
        year::Int
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

class (Eq a, Show a) => PublicationManager a where
    getPublicationTypeByTitle :: String -> [a] -> Maybe String
    getPublicationByAuthorWithPredicate :: String -> (a -> Bool) -> [a] -> [a]
    getSingleAutoredPublicationsByAuthor :: String -> [a] -> [a]
    listAllPublishers :: [a] -> [String]
    listAllJournals :: [a] -> [String]
    listAllConferences :: [a] -> [String]
    getStatistics :: [a] -> [(String, Int)]
    (&) :: a -> a -> [String]
    (>!) :: a -> a -> Bool

instance PublicationManager Publication where
    getPublicationTypeByTitle targetTitle publicationList =
        case [pub | pub <- publicationList, title pub == targetTitle] of
            [Book {}]    -> Just "Book"
            [Article {}] -> Just "Article"
            [Thesis {}]  -> Just "Thesis"
            []           -> Nothing
            _            -> error "Several publications with the same title"

    getPublicationByAuthorWithPredicate targetAuthor predicate publicationList = 
        foldl (\acc pub -> if elem targetAuthor (authors pub) && predicate pub then pub:acc else acc) [] publicationList

    getSingleAutoredPublicationsByAuthor targetAuthor publicationList = filter (\pub -> [targetAuthor] == (authors pub)) publicationList

    listAllPublishers publicationList = 
        foldl (\acc pub -> if elem pub acc then acc else acc ++ [pub]) [] [publisher pub | pub@(Book {}) <- publicationList]

    listAllJournals publicationList = 
        foldl (\acc pub -> if elem pub acc then acc else acc ++ [pub]) [] [journal pub | pub@(Article {}) <- publicationList]

    listAllConferences publicationList = 
        foldl (\acc pub -> if elem pub acc then acc else acc ++ [pub]) [] [conference pub | pub@(Thesis {}) <- publicationList]

    getStatistics publicationList =
        zip ["Books", "Articles", "Theses"] (foldl countPublication [0, 0, 0] publicationList)
        where countPublication (books:articles:theses:_) pub = case pub of
                Book {}    -> [books + 1, articles, theses]
                Article {} -> [books, articles + 1, theses]
                Thesis {}  -> [books, articles, theses + 1]

    pub1 & pub2 = foldl (\acc pub -> if elem pub (authors pub2) then acc ++ [pub] else acc) [] (authors pub1)
    pub1 >! pub2 = (year pub1) > (year pub2)

instance Semigroup Publication where
      p1 <> p2 = Book { authors = authors p1 ++ authors p2, 
        title = title p1 ++ ", " ++ title p2, 
        city = "",
        publisher = "",
        year = max (year p1) (year p2)}

instance Monoid Publication where
    mempty = Book [] "" "" "" 0

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Show)
data Box a = Empty |  Element a (Box a) deriving (Show)

instance Foldable Tree where
    foldMap f (Leaf x) = f x
    foldMap f (Branch l r) = foldMap f l `mappend` foldMap f r

instance Foldable Box where
    foldMap f (Element a box) = f a `mappend` foldMap f box
    foldMap f (Empty) = mempty
    