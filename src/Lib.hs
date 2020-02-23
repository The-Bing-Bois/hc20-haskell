module Lib
    ( readData,
    solve,
    writeAnswer
    ) where
--- IMPORTS ------------------------------------------------------
import qualified Data.IntMap.Strict as M
import qualified Data.List as L
import qualified Data.Set as S

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
    compare (Book _ a _) (Book _ b _) = compare a b

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
          newBm = reverse $ L.sort $ M.elems transitBm

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
mapScores []     _ z = z
mapScores (b:bs) n z = mapScores bs (n+1) (M.insert n (Book n b []) z)

-- map the Library into the Library map and add an index reference to any Book it contains
mapLibrary :: [String] -> M.IntMap Book -> M.IntMap Library -> (M.IntMap Book, M.IntMap Library)
mapLibrary [] bm z = (bm, z)
mapLibrary (i:b:ls) bm z = mapLibrary ls newBm newZ
    where [_, signUpTime, bookPerDay] = read <$> words i
          lid = M.size z
          newZ = M.insert lid (Library lid signUpTime bookPerDay (-1)) z
          newBm = updateFromLibrary (read <$> words b) bm lid
          updateBook lid (Just (Book bid score blibs)) = Just (Book bid score (blibs ++ [lid]))
          updateBook _ Nothing = Nothing
          updateFromLibrary xs bmap lid = foldr (M.alter (updateBook lid)) bmap xs
mapLibrary [x] bm z = (bm, z)

-- solve the problem given the tuple and sort by opened library, requires books sorted by score
solve :: ([Book], M.IntMap Library, Days) -> [LibraryOpened]
solve (b, lbs, d) = filter (\(LibraryOpened _ l _ _ _) -> (length l) > 0) . L.sort . M.elems $ o
    where (_, _, _, _, o) = solveFull b lbs d Nothing M.empty

-- solve the given problem exiting on Day or Book finished
solveFull :: [Book] -> M.IntMap Library -> Days -> Maybe Library -> M.IntMap LibraryOpened
              -> ([Book], M.IntMap Library, Days, Maybe Library, M.IntMap LibraryOpened)
solveFull [] _ _ _ opened = ([], M.empty, -1, Nothing, opened)
-- solveFull _  _ 0 _ opened = ([], M.empty, -1, Nothing, opened) -------- looks like w/o this doesn't terminate
solveFull (b:bs) libs d opening opened = solveFull newBs newLib newD newOpening newOpened
    where (newBs, newLib, newD, newOpening, newOpened) = tic . pass2 . pass1 $ (b:bs, libs, d, opening, opened)

-- decrease the Day and reset currently scanned Books
tic :: ([Book], M.IntMap Library, Days, Maybe Library, M.IntMap LibraryOpened)
       -> ([Book], M.IntMap Library, Days, Maybe Library, M.IntMap LibraryOpened)
tic (bs, libs, d, opening, opened) = (bs, libs, d-1, opening, M.map updateLibrary opened)
    where updateLibrary (LibraryOpened id books max current n) = LibraryOpened id (books ++ current) max [] n

-- try putting more books as possible in the opened libraries
pass1 (bs, libs, d, opening, opened) = (newBs, libs, d, opening, newOpened)
    where (newBs, newOpened) = tryStuffingBooks bs opened M.empty

appendBookToLibrary bid (Just(LibraryOpened id blst max clst n)) = Just (LibraryOpened id blst max (clst ++ [bid]) n)
appendBookToLibrary bid Nothing                                  = Nothing

tryFindLibrary [] opened            = Nothing
tryFindLibrary (lid:blid) opened    = res
                                      where blibset = S.fromList (lid:blid)
                                            feels (LibraryOpened _ _ _ a _) (LibraryOpened _ _ _ b _) = compare (length a) (length b)
                                            res = case ( L.sortBy feels $ filter (\(LibraryOpened lid _ _ _ _) -> S.member lid blibset) $ M.elems opened ) of
                                                            []      -> Nothing
                                                            (x: _)  -> Just x

tryStuffingBooks [] opened filled                         = ([], (M.union opened filled))
tryStuffingBooks ((Book bid score blid):bs) opened filled = case (M.size opened) of
    0 -> ((Book bid score blid):bs, filled)
    _ ->  case (tryFindLibrary blid opened) of
            Nothing -> ((Book bid score blid):rbs, rf)
                where (rbs, rf) = tryStuffingBooks bs opened filled
            Just (LibraryOpened lid blst max clst n) -> x
                where x
                       | (length clst) == max - 1 = tryStuffingBooks bs (M.delete lid opened) (M.insert lid (LibraryOpened lid blst max (bid:clst) n) filled)
                       | (length clst) <  max - 1 = tryStuffingBooks bs (M.alter (appendBookToLibrary bid) lid opened) filled

-- try to open a new library if possible
pass2 (bs, libs, d, opening, opened) = (bs, newLibs, d, newOpening, newOpened)
    where (newOpening, newLibs, newOpened) = tryOpenBestLibrary bs libs opening opened

tryGetBestLibrary []          _ = Nothing
tryGetBestLibrary (bl:blid) lbs = res
    where blibset = S.fromList (bl:blid)
          res = case ( L.sort $ filter (\(Library lid _ _ _) -> S.member lid blibset) $ M.elems lbs ) of
                []    -> Nothing
                (x:_) -> Just x

tryOpenBestLibrary :: [Book] -> M.IntMap Library -> Maybe Library -> M.IntMap LibraryOpened
                        -> (Maybe Library, M.IntMap Library, M.IntMap LibraryOpened)
tryOpenBestLibrary []                                 lbs Nothing s = (Nothing, lbs, s)
tryOpenBestLibrary ((Book bid score []):bs)           lbs Nothing s = tryOpenBestLibrary bs lbs Nothing s
tryOpenBestLibrary ((Book bid score (blid:blist)):bs) lbs Nothing s = case (M.size lbs) of
    0 -> (Nothing, lbs, s)
    _ -> case (tryGetBestLibrary (blid:blist) lbs) of
            Nothing -> tryOpenBestLibrary bs lbs Nothing s
            Just (Library lid t m _) -> (Just (Library lid t m (M.size s)), (M.delete lid lbs), s)
tryOpenBestLibrary bs lbs (Just (Library lid t m n)) s
    | t > 1     = (Just (Library lid (t-1) m n), lbs, s)
    | otherwise = tryOpenBestLibrary bs lbs Nothing (M.insert lid (LibraryOpened lid [] m [] n) s)


