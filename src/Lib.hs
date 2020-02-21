module Lib
    ( readData,
    solve,
    writeAnswer
    ) where
--- IMPORTS ------------------------------------------------------
import qualified Data.IntMap.Strict as M
import qualified Data.List as L

--- TYPES & DATA -------------------------------------------------

-- type for typechecking
type Days        = Int
type MaxBooks    = Int
type NoOpen      = Int
type LibraryId   = Int       -- [1, 1000]
type BookId      = Int       -- [1, 100000]
type Score       = Int       -- [1, 800]
type SignUpTime  = Int       -- [1, ?]

-- type of Books, sortable by Score
data Book = Book BookId Score [LibraryId] deriving (Show, Eq)
instance Ord Book where
    compare (Book _ a _) (Book _ b _) = compare b a

-- type of Library, sortable by SignUpTime
data Library = Library LibraryId SignUpTime MaxBooks NoOpen deriving (Eq, Show)
instance Ord Library where
    compare (Library _ a _ _) (Library _ b _ _) = compare a b

-- type of LibraryOpened, sortable by opening order
data LibraryOpened = LibraryOpened LibraryId [BookId] MaxBooks [BookId] NoOpen deriving (Eq, Show)
instance Ord LibraryOpened where
    compare (LibraryOpened _ _ _ _ a) (LibraryOpened _ _ _ _ b) = compare a b

--- FUNCTIONS ----------------------------------------------------

-- takes an input and produce a tuple to process
readData :: String -> ([Book], M.IntMap Library, Days)
readData input = (newBm, newLib, (read d))
    where (l:bs:ls) = lines input
          [_, _, d] = words l
          (transitBm, newLib) = mapLibrary ls (mapScores (read <$> words bs) 0 M.empty) M.empty
          newBm = L.sort $ M.elems transitBm

-- takes an array of LibraryOpened and create an output
writeAnswer :: [LibraryOpened] -> String
writeAnswer lst = (show $ length lst) ++ "\n" ++ (writeLibrary sortedList "")
    where sortedList = L.sort lst
          writeLibrary [] out = out
          writeLibrary ((LibraryOpened id bs _ _ _):lbs) out =
                writeLibrary lbs (out ++ (show id) ++ " "
                    ++ (show $ length bs) ++ "\n" ++ (unwords $ show <$> bs) ++ "\n")

-- map the Scores into the Book map
mapScores :: [Int] -> Int -> M.IntMap Book -> M.IntMap Book
mapScores [] _ z = z
mapScores (b:bs) n z = mapScores bs (n+1) (M.insert n (Book n b []) z)

-- map the Library into the Library map and add an index reference to any Book it contains
mapLibrary :: [String] -> M.IntMap Book -> M.IntMap Library -> (M.IntMap Book, M.IntMap Library)
mapLibrary [] bm z = (bm, z)
mapLibrary (i:b:ls) bm z = mapLibrary ls newBm newZ
    where [_, bookPerDay, signUpTime] = read <$> words i
          lid = M.size z
          newZ = M.insert lid (Library lid signUpTime bookPerDay 0) z
          newBm = updateFromLibrary (read <$> words b) bm lid
          updateBook lid (Just (Book bid score blibs)) = Just (Book bid score (lid:blibs))
          updateBook _ Nothing = Nothing
          updateFromLibrary xs bmap lid = foldr (M.alter (updateBook lid)) bmap xs

-- solve the problem given the tuple and sort by opened library, requires books sorted by score
solve :: ([Book], M.IntMap Library, Days) -> [LibraryOpened]
solve (b, lbs, d) = (L.sort $ M.elems o)
    where (_, _, _, _, o) = solveFull b lbs d Nothing M.empty

-- solve the given problem exiting on Day or Book finished
solveFull :: [Book] -> M.IntMap Library -> Days -> Maybe Library -> M.IntMap LibraryOpened -> ([Book], M.IntMap Library, Days, Maybe Library, M.IntMap LibraryOpened)
solveFull [] _ _ _ opened = ([], M.empty, -1, Nothing, opened)
solveFull _  _ 0 _ opened = ([], M.empty, -1, Nothing, opened)
solveFull bs libs d opening opened = solveFull newBs newLib newD newOpening newOpened
    where (newBs, newLib, newD, newOpening, newOpened) = tic . pass2 . pass1 $ (bs, libs, d, opening, opened)

-- decrease the Day and reset currently scanned Books
tic :: ([Book], M.IntMap Library, Days, Maybe Library, M.IntMap LibraryOpened) -> ([Book], M.IntMap Library, Days, Maybe Library, M.IntMap LibraryOpened)
tic (bs, libs, d, opening, opened) = (bs, libs, d-1, opening, M.map updateLibrary opened)
    where updateLibrary (LibraryOpened id books max current n) = LibraryOpened id (books ++ current) max [] n

-- try putting more books as possible in the opened libraries
pass1 (bs, libs, d, opening, opened) = (newBs, libs, d, opening, newOpened)
    where (newBs, _, newOpened) = tryStuffingBooks bs opened M.empty

appendBookToLibrary bid (Just(LibraryOpened id blst max clst n)) = Just (LibraryOpened id blst max (bid:clst) n)
appendBookToLibrary bid Nothing                                  = Nothing

tryFindLibrary [] opened            = Nothing
tryFindLibrary (lid:blid) opened    = case (M.lookup lid opened) of
    Just (LibraryOpened a b c d e) -> Just(LibraryOpened a b c d e)
    Nothing                        -> tryFindLibrary blid opened

tryStuffingBooks [] opened filled = ([], M.empty, (M.union opened filled))
tryStuffingBooks ((Book bid score blid):bs) opened filled = case (M.size opened) of
    0 -> ((Book bid score blid):bs, M.empty, (M.union opened filled))
    _ ->  case (tryFindLibrary blid opened) of
            Nothing -> ((Book bid score blid):rbs, M.empty, (M.union ro rf))
                where (rbs, ro, rf) = tryStuffingBooks bs opened filled
            Just (LibraryOpened lid blst max clst n) -> x
                where x
                       | (length clst) == max - 1 = tryStuffingBooks bs (M.delete lid opened) (M.insert lid (LibraryOpened lid blst max (bid:clst) n) filled)
                       | (length clst) < max  = tryStuffingBooks bs (M.alter (appendBookToLibrary bid) lid opened) filled

-- if opened tick the Library otherwise open it and open the best new library
pass2 (bs, libs, d, opening, opened) = (bs, newLibs, d, newOpening, newOpened)
    where (newOpening, newLibs, newOpened) = tryOpenBestLibrary bs libs opening opened

tryGetBestLibrary [] lbs = Nothing
tryGetBestLibrary (bl:blid) lbs = 
   case ( filter (\(Library id _ _ _) -> any (\(xid) -> id == xid) (bl:blid)) $ L.sort $ M.elems lbs ) of
        []     -> Nothing
        (x:xs) -> Just x

tryOpenBestLibrary :: [Book] -> M.IntMap Library -> Maybe Library -> M.IntMap LibraryOpened -> (Maybe Library, M.IntMap Library, M.IntMap LibraryOpened)
tryOpenBestLibrary [] lbs _ s = (Nothing, lbs, s)
tryOpenBestLibrary ((Book bid score []):bs) lbs Nothing s         = tryOpenBestLibrary bs lbs Nothing s
tryOpenBestLibrary ((Book bid score (lid:blid)):bs) lbs Nothing s = case (tryGetBestLibrary (lid:blid) lbs) of
    Nothing -> tryOpenBestLibrary bs lbs Nothing s 
    Just (Library lid t m n) -> (Just (Library lid t m (M.size s)), (M.delete lid lbs), s)
tryOpenBestLibrary (b:bs) lbs (Just (Library lid t m n)) s
    | t > 0 = (Just (Library lid (t-1) m n), lbs, s)
    | otherwise = res
                    where (x, y, z) = tryOpenBestLibrary bs (M.delete lid lbs) Nothing M.empty
                          res = (x, y, M.insert lid (LibraryOpened lid [] m [] n) s)


