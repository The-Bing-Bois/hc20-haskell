{-# LANGUAGE BangPatterns #-}

module Lib
    ( readData,
    solve,
    writeAnswer
    ) where
--- IMPORTS ------------------------------------------------------
import qualified Data.IntMap.Strict as M
import qualified Data.List as L
import qualified Data.Set as S

import Debug.Trace

--- TYPES & DATA -------------------------------------------------

-- type for typechecking
type Days        = Int
type MaxBooks    = Int
type NoOpen      = Int
type LibraryId   = Int
type BookId      = Int
type Score       = Int
type SignUpTime  = Int

-- type of Books, sortable by Score
data Book = Book BookId Score [LibraryId] deriving (Show, Eq)
instance Ord Book where
    compare (Book _ a _) (Book _ b _) = compare a b

-- type of Library, sortable by SignUpTime
data Library = Library LibraryId SignUpTime MaxBooks NoOpen deriving (Eq, Show)
instance Ord Library where
    compare (Library _ a _ _) (Library _ b _ _) = compare a b

-- type of LibraryOpened, sortable by opening order
data LibraryOpened = LibraryOpened LibraryId [BookId] MaxBooks [BookId] NoOpen deriving (Eq, Show)
instance Ord LibraryOpened where
    compare (LibraryOpened _ _ _ _ a) (LibraryOpened _ _ _ _ b) = compare a b

data LibraryInstance = LibraryInstance
    { books   :: ![Book]
    , lbs     :: !(M.IntMap Library)
    , days    :: !Days
    , opening :: !(Maybe Library)
    , opened  :: !(M.IntMap LibraryOpened)
    } deriving (Show)

--- FUNCTIONS ----------------------------------------------------

-- takes an input and produce a tuple to process
readData :: String -> ([Book], M.IntMap Library, Days)
readData input = (newBm, newLib, (read d))
    where (l:bs:ls) = lines input
          [_, _, d] = words l
          (transitBm, newLib) = mapLibrary ls (mapScores (read <$> words bs) 0 M.empty) M.empty
          newBm = reverse $ L.sort $ M.elems transitBm

-- takes an array of LibraryOpened and create an output
writeAnswer :: [LibraryOpened] -> String
writeAnswer lst = (show $ length lst) ++ "\n" ++ (writeLibrary lst "")
    where writeLibrary [] out = out
          writeLibrary ((LibraryOpened id bs _ _ _) : lbs) out =
                writeLibrary lbs (out ++ (show id) ++ " "
                    ++ (show $ length bs) ++ "\n" ++ (unwords $ show <$> bs) ++ "\n")

-- map the Scores into the Book map
mapScores :: [Int] -> Int -> M.IntMap Book -> M.IntMap Book
mapScores []     _ z = z
mapScores (b:bs) n z = mapScores bs (n+1) (M.insert n (Book n b []) z)

-- map the Library into the Library map and add an index reference to any Book it contains
mapLibrary :: [String] -> M.IntMap Book -> M.IntMap Library -> (M.IntMap Book, M.IntMap Library)
mapLibrary [] bm z = (bm, z)
mapLibrary (i:b:ls) bm z = mapLibrary ls newBm newZ
    where [_, signUpTime, bookPerDay] = read <$> words i
          lid = M.size z
          newZ = M.insert lid (Library lid signUpTime bookPerDay (-1)) z
          updateBook lid (Just (Book bid score blibs)) = Just (Book bid score (blibs ++ [lid]))
          updateBook _ Nothing = Nothing
          updateFromLibrary xs bmap lid = foldr (M.alter (updateBook lid)) bmap xs
          newBm = updateFromLibrary (read <$> words b) bm lid
mapLibrary [_] bm z = (bm, z)

-- solve the problem given the tuple and sort by opened library, requires books sorted by score
solve :: ([Book], M.IntMap Library, Days) -> [LibraryOpened]
solve (b, lbs, d) = filter (\(LibraryOpened _ a _ _ _) -> (length a) > 0) . L.sort . M.elems $ o
    where bs = filter (\(Book _ _ l) -> (length l) > 0) b
          (LibraryInstance _ _ _ _ o) = solveFull $! LibraryInstance bs lbs (d+1) Nothing M.empty

-- solve the given problem exiting on Day or Book finished
-- solveFull ([], _, _, _, opened) = ([], M.empty, -1, Nothing, opened)
solveFull :: LibraryInstance -> LibraryInstance
-- solveFull !(LibraryInstance _ _ 0 _ opened) = LibraryInstance [] M.empty (-1) Nothing opened
solveFull !tup@(LibraryInstance _ _ d _ _)
    | d == 0   = tup
    | otherwise = solveFull $! tic . pass2 . pass1 $! tup

-- decrease the Day and reset currently scanned Books
tic :: LibraryInstance -> LibraryInstance
tic !(LibraryInstance bs libs d opening opened) = (LibraryInstance bs libs (d-1) opening (M.map updateLibrary opened))
    where updateLibrary (LibraryOpened id books max current n) = LibraryOpened id (books ++ current) max [] n

-- try putting more books as possible in the opened libraries
pass1 :: LibraryInstance -> LibraryInstance
pass1 !(LibraryInstance bs libs d opening opened) = (LibraryInstance newBs libs d opening newOpened)
    where (newBs, newOpened) = tryStuffingBooks bs opened M.empty

appendBookToLibrary :: Int -> Maybe LibraryOpened -> Maybe LibraryOpened
appendBookToLibrary !bid !(Just(LibraryOpened id blst max clst n)) = Just (LibraryOpened id blst max (clst ++ [bid]) n)
appendBookToLibrary _   Nothing                                    = Nothing

tryFindLibrary :: [Int] -> M.IntMap LibraryOpened -> Maybe LibraryOpened
tryFindLibrary ![]         _         = Nothing
tryFindLibrary !(lid:blid) !opened    = case (filter (flip M.member $ opened) (lid:blid)) of
                                        []      -> Nothing
                                        (x:_)  -> M.lookup x opened

tryStuffingBooks :: [Book] -> M.IntMap LibraryOpened -> M.IntMap LibraryOpened
                    -> ([Book], M.IntMap LibraryOpened)
tryStuffingBooks ![]                           !opened !filled = ([], (M.union opened filled))
tryStuffingBooks !(!(Book bid score blid) : bs) !opened !filled = case (M.null opened) of
    True  -> ((Book bid score blid) : bs, filled)
    False ->  case (tryFindLibrary blid opened) of
        Nothing -> ((Book bid score blid) : rbs, rf)
            where (rbs, rf) = tryStuffingBooks bs opened filled
        Just (LibraryOpened lid blst max clst n) -> x
            where x
                   | (length clst) == max - 1 = tryStuffingBooks bs
                                                                 (M.delete lid opened)
                                                                 (M.insert lid (LibraryOpened lid blst max (clst ++ [bid]) n) filled)
                   | (length clst) <  max - 1 = tryStuffingBooks bs
                                                                 (M.alter (appendBookToLibrary bid) lid opened)
                                                                 filled
                   | otherwise                = tryStuffingBooks ((Book bid score blid) : bs)
                                                                 (M.delete lid opened)
                                                                 (M.insert lid (LibraryOpened lid blst max clst n) filled)

-- try to open a new library if possible
pass2 :: LibraryInstance -> LibraryInstance
pass2 !(LibraryInstance bs !libs d !opening !opened) = (LibraryInstance bs newLibs d newOpening newOpened)
    where (newOpening, newLibs, newOpened) = tryOpenBestLibrary bs libs opening opened

tryOpenBestLibrary :: [Book] -> M.IntMap Library -> Maybe Library -> M.IntMap LibraryOpened
                        -> (Maybe Library, M.IntMap Library, M.IntMap LibraryOpened)
tryOpenBestLibrary ![] !lbs Nothing !s = (Nothing, lbs, s)
tryOpenBestLibrary !bs !lbs Nothing !s =
    case (M.null lbs) of
        True  -> (Nothing, lbs, s)
        False -> x
            where (Library lid t m _) = L.minimum . M.elems $ lbs
                  x
                    | t > 1     = (Just (Library lid (t-1) m (M.size s))
                                , (M.delete lid lbs)
                                , s)
                    | otherwise = tryOpenBestLibrary bs
                                                    (M.delete lid lbs)
                                                    Nothing
                                                    (M.insert lid (LibraryOpened lid [] m [] (M.size s)) s)
tryOpenBestLibrary !bs !lbs !(Just (Library lid t m n)) !s
    | t > 1     = (Just (Library lid (t-1) m n)
                   , lbs
                   , s)
    | otherwise = tryOpenBestLibrary bs
                                     lbs
                                     Nothing
                                     (M.insert lid (LibraryOpened lid [] m [] n) s)


