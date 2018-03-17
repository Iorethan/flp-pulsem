
mhead :: [a] -> Maybe a
mhead [] = Nothing
mhead (x:xs) = Just x

mtail :: [a] -> Maybe a
mtail [] = Nothing
mtail [x] = Just x
mtail (x:xs) = mtail xs

mtail2 :: [a] -> Maybe a
mtail2 as = mtail2' as Nothing

mtail2' :: [a] -> Maybe a -> Maybe a
mtail2' [] prev = prev
mtail2' (x:xs) _ = mtail2' xs (Just x)

mtail3 :: [a] -> Maybe a
mtail3 = mhead . rev

rev :: [a] -> [a]
rev xs = rev' xs []

rev' :: [a] -> [a] -> [a]
rev' [] as = as
rev' (x:xs) as = rev' xs (x:as)

takeN :: Int -> [a] -> [a]
takeN _ [] = []
takeN n (x:xs)
    | n <= 0 = []
    | otherwise = x:(takeN (n - 1) xs)

dropN :: Int -> [a] -> [a]
dropN _ [] = []
dropN n (x:xs)
    | n <= 0 = (x:xs)
    | otherwise = dropN (n - 1) xs

replac :: Int -> Char -> Char -> [String] -> [String]
replac _ _ _ [] = []
replac n a b (x:xs) = (replacS n a b x):(replac n a b xs)

replacS :: Int -> Char -> Char -> String -> String
replacS n a b x = cnct (takeN (n - 1) x) (rpl (drop (n - 1) x))
    where rpl [] = []
          rpl (n:ns)
            | n == a = (b:ns)
            | otherwise = (n:ns)

cnct :: [a] -> [a] -> [a]
cnct [] b = b
cnct (a:as) b = a:(cnct as b) 

ssplitAt :: Int -> [a] -> ([a], [a])
ssplitAt n xs = (takeN n xs, dropN n xs)

rpl :: Int -> String -> String -> [String] -> [String]
rpl _ _ _ [] = []
rpl n a b (x:xs) = (rpl' n a b x):(rpl n a b xs)

rpl' :: Int -> String -> String -> String -> String
rpl' n fnd pst x = if b == fnd then a ++ pst ++ c else a ++ b ++ c
    where (a,b') = ssplitAt n x
          (b,c) = ssplitAt (n + length(fnd) - 1) b'

mysort :: Ord a => [a] -> [a]
mysort [] = []
mysort (x:xs) = insert x (mysort xs)

insert :: Ord a => a -> [a] -> [a]
insert a [] = [a]
insert a (x:xs)
    | a < x = (a:x:xs)
    | otherwise = (x:insert a xs)

type Line = (Int, String)
type File3 = [Line]

data Line2 = Line2 Int String
    deriving(Show, Eq)
data File2 = File2 [Line2]
    deriving(Show, Eq)

addLine :: Line2 -> File2 -> File2
addLine ln (File2 lns) = File2 (ln:lns)

rf :: String -> IO File3
rf pth = (readFile pth) >>= (return . toFile . prune . lines)

rf2 :: String -> IO File2
rf2 pth = (readFile pth) >>= (return . toFile2 . prune . lines)

toFile :: [String] -> File3
toFile = toFile' 1

toFile' :: Int -> [String] -> File3
toFile' _ [] = []
toFile' n (x:xs) = (n, x):(toFile' (n + 1) xs)

toFile2 :: [String] -> File2
toFile2 = toFile2' 1

toFile2' :: Int -> [String] -> File2
toFile2' _ [] = File2 []
toFile2' n (x:xs) = addLine (Line2 n x) (toFile2' (n + 1) xs)

prune :: [String] -> [String]
prune [] = []
prune (x:xs) = if dropped /= "" then (dropped:prune xs) else prune xs
    where dropped = dropTailSpace x

dropTailSpace :: String -> String
dropTailSpace = rev . dropHeadSpace . rev

dropHeadSpace :: String -> String
dropHeadSpace "" = ""
dropHeadSpace (' ':xs) = dropHeadSpace xs
dropHeadSpace xs = xs

data Filesystem = Filesystem String [Item]
    deriving(Show)
data Item = File String | Folder String [Item]
    deriving(Show)

a = File "a"
b = File "b"
c = File "c"
fs = Filesystem "" [Folder "tmp" [a, b, Folder "xv" [b]], Folder "var"[a, c], a, b]

pathToFiles :: Filesystem -> String -> [String]
pathToFiles (Filesystem root list) name = searchForFiles list root name []

searchForFiles :: [Item] -> String -> String -> [String] -> [String]
searchForFiles [] _ _ curr = curr
searchForFiles (x:xs) path name curr = processItem x path name curr ++ searchForFiles xs path name curr

processItem :: Item -> String -> String -> [String] -> [String]
processItem (File nm) path name curr = if nm == name then (path ++ "/" ++ nm):curr else curr
processItem (Folder nm list) path name curr = searchForFiles list (path ++ "/" ++ nm) name curr
