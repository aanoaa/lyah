module PCheck (main) where

import           Data.List        (isPrefixOf, isSuffixOf)
import           System.Directory (listDirectory)

-- | determine filename is `host.*.csv` or not.
-- >>> isHostFile "host.csv"
-- False
-- >>> isHostFile "host.foo.csv"
-- True
isHostFile :: FilePath -> Bool
isHostFile "host.csv" = False
isHostFile x          = isPrefixOf "host." x && isSuffixOf ".csv" x

-- | first columns of given csv file content.
-- >>> fstCols ["prod-abc-opx01,10.0.0.10","# skip this line","","prod-abc-opx02,10.0.0.11"]
-- ["prod-abc-opx01","prod-abc-opx02"]
fstCols :: [String] -> [String]
fstCols lines =
  takeWhile (/= ',')
    <$> filter
      ( and
          . sequence
            [ not . isPrefixOf "#",
              (/= "")
            ]
      )
      lines

-- | list group by given string.
-- | group is the value separated by the '-' character
-- >>> listGroup "prod-abc-opx01"
-- ["prod","abc","opx","prod-abc","abc-opx","prod-abc-opx"]
listGroup :: String -> [String]
listGroup ""   = []
listGroup s = [f1, f2, f3, f4, f5, f6]
  where
    pick = takeWhile (/= '-')
    skip = drop 1 . dropWhile (/= '-')
    f1 = pick s
    f2 = pick . skip $ s
    f3 = takeWhile (`notElem` ['0' .. '9']) . pick . skip . skip $ s
    f4 = concat [f1, "-", f2]
    f5 = concat [f2, "-", f3]
    f6 = concat [f1, "-", f2, "-", f3]

-- | split lines then concat into a list.
-- >>> stripLn ["foo\nbar\n","baz\n"]
-- ["foo","bar","baz"]
stripLn :: [String] -> [String]
stripLn = foldr ((<>) . lines) []

-- >>> groupFromPolicy ["# dmz -> q","abc-opx,abc-ipx,8080","# q -> secure","abc-ipx,abc-app,8080","# q -> secure; undefined","abc-ipx,abc-prx,8080"]
-- ["abc-opx","abc-ipx","abc-ipx","abc-app","abc-ipx","abc-prx"]
groupFromPolicy :: [String] -> [String]
groupFromPolicy xs = do
  s <- filter (not . isPrefixOf "#") xs
  [f1 s, f2 s]
  where
    pick = takeWhile (/= ',')
    skip = drop 1 . dropWhile (/= ',')
    f1 = pick
    f2 = pick . skip

-- | find <dirpath> -name 'host.*.csv'
-- | grouping by '-' separator
-- | find undefined group in "policy.csv"
main :: FilePath -> IO ()
main dirpath = do
  list <- listDirectory dirpath
  let basename = filter isHostFile list
  let files = [dirpath <> "/" <> f | f <- basename]
  s <- mapM readFile files
  let cols = fstCols (stripLn s)
  let g = concatMap listGroup cols

  p <- readFile (dirpath <> "/policy.csv")
  let policy = lines p
  let pg = groupFromPolicy policy

  let r = (`elem` g) <$> pg
  let ok = if False `elem` r then "Not OK" else "OK"
  print ok
