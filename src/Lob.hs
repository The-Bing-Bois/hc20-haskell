{-# LANGUAGE BangPatterns #-}

module Lob
    ( readData,
    solve,
    writeAnswer,
    ) where

--- IMPORTS ----------------------------------------------

import qualified Data.IntMap.Strict as M
import qualified Data.List as L
import qualified Data.Set as S

import qualified Debug.Trace as D

--- TYPES & DATA -----------------------------------------

-- type for typechecking
type Days       = Int
type MaxBooks   = Int
type LibraryId  = Int
type BookId     = Int
type Score      = Int
type SignUpTime = Int

-- type of Books, sortable by Score
data Book = Book BookId Score deriving (Eq)
instance Ord Book where
    compare (Book _ a) (Book _ b) = compare a b
instance Show Book where
    show (Book id _) = show id

mapScores :: [Int] -> Int -> M.IntMap Book -> M.IntMap Book
mapScores []     _ z = z
mapScores (b:bs) n z = mapScores bs (n + 1) (M.insert n (Book n b) z)

bookId :: Book -> BookId
bookId (Book id _) = id

-- type of Library, sortable by SignUpTime
data Library = Library LibraryId SignUpTime MaxBooks [Book] deriving (Show, Eq)
instance Ord Library where
    compare (Library _ a _ _) (Library _ b _ _) = compare a b

scoreLibrary :: Library -> Days -> Int
scoreLibrary (Library _ up booksPerDay bs) days = L.foldl' sumBooks 0 (take (booksPerDay * (days - up)) bs) 
    where sumBooks t (Book _ s) = t + s

trimInDays :: Days -> Library -> Library
trimInDays days (Library a up booksPerDay bs) = 
    (Library a up booksPerDay (take (booksPerDay * (days - up)) bs))

mapLibrary :: [String] -> M.IntMap Book -> M.IntMap Library -> M.IntMap Library
mapLibrary []  _ z = z
mapLibrary [_] _ z = z
mapLibrary (i : b : ls) bs z = mapLibrary ls bs newZ
    where [_, signUpTime, bookPerDay] = read <$> words i
          lid = M.size z
          books = S.fromList $ read <$> words b
          bookList = L.reverse . L.sort . M.elems
                        $ M.filter (\(Book id _) -> S.member id books) bs
          newZ = M.insert lid (Library lid signUpTime bookPerDay bookList) z

-- type of LibraryInstance for the loop
data LibraryInstance = LibraryInstance
    { lbs       :: [Library]
    , days      :: Days
    } deriving (Show)

--- FUNCTIONS --------------------------------------------

-- takes an input and produce an instance
readData :: String -> LibraryInstance
readData input = LibraryInstance lbs d
    where (l:bs:ls) = lines input
          [_, _, d] = read <$> words l
          books     = mapScores (read <$> words bs) 0 M.empty
          lbs       = L.sort . M.elems $ mapLibrary ls books M.empty

-- takes an array of Library and create an output
writeAnswer :: [Library] -> String
writeAnswer lst = (show $ length lst) <> "\n" <> (writeLibrary lst "")
    where writeLibrary [] out = out
          writeLibrary ((Library id _ _ bs) : lbs) out =
                writeLibrary lbs
                             (out <> (show id) <> " "
                              <> (show $ length bs) <> "\n"
                              <> (unwords $ show <$> bs) <> "\n")

-- solve the problem given an instance
solve :: LibraryInstance -> [Library]
solve (LibraryInstance lbs d) = filter (\(Library _ _ _ a) -> not . null $ a) solution
    where solution = solveFull (LibraryInstance lbs (d + 1)) []

solveFull :: LibraryInstance -> [Library] -> [Library]
solveFull (LibraryInstance [] days) solution = solution
solveFull (LibraryInstance _  0) solution = solution
solveFull !i@(LibraryInstance lbs days) !solution =
    solveFull (D.trace ("added: " <> (show $ scoreLibrary newCandidate days) 
                        <> " days remaining: " <> (show $ newDays) 
                        <> " libraries: " <> (show $ length lbs))
              (LibraryInstance newLibrary newDays)) (solution ++ [newCandidate])
        where newCandidate = chooseBestCandidate i
              newLibrary   = updateLibraryFromCandidate lbs   newCandidate
              newDays      = updateDaysFromCandidate    days  newCandidate

chooseBestCandidate :: LibraryInstance -> Library
chooseBestCandidate (LibraryInstance lbs days) = trimInDays days $ L.maximumBy (compareScore days) lbs
    where compareScore days a@(Library _ ta _ _) b@(Library _ tb _ _) = compare ((scoreLibrary a days)*(tb)) ((scoreLibrary b days)*(ta))

updateLibraryFromCandidate :: [Library] -> Library -> [Library]
updateLibraryFromCandidate lbs (Library cid _ _ candidateBooks) = filterEmpty $ filterWithCandidate <$> filterCandidate lbs
        where bookSet = S.fromList $ bookId <$> candidateBooks
              filterWithCandidate (Library a b c books) =
                    (Library a b c (filter (\(Book id _) -> not (S.member id bookSet)) books))
              filterCandidate libs = filter (\(Library id _ _ _) -> not (id == cid)) libs
              filterEmpty libs = filter (\(Library _ _ _ a) -> (length a) > 0) libs

updateDaysFromCandidate days (Library _ t _ _)
    | (days - t) > 0 = (days - t)
    | otherwise      = 0

