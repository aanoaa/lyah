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

-- | split by '-'
-- >>> words' "foo-bar-baz"
-- ["foo","bar","baz"]
words' :: String -> [String]
words' s = case dropWhile (== '-') s of
  "" -> []
  s' -> w : words' s''
    where
      (w, s'') = break (== '-') s'

-- | list group by given string.
-- | group is the value separated by the '-' character
-- >>> listGroup "prod-abc-opx01"
-- ["prod","abc","opx","prod-abc","abc-opx","prod-abc-opx"]
listGroup :: String -> [String]
listGroup ""   = []
listGroup name
  | tokenl /= 3 = [name]
  | otherwise = rename <> [x <> "-" <> y | (x, y) <- zip rename (tail rename)] <> [replace ' ' '-' (unwords rename)]
  where
    token = words' name
    tokenl = length token
    -- 마지막 요소에서 숫자를 뗀다. baz01 -> baz
    rename = [if i /= tokenl then x else [c | c <- x, c `notElem` ['0' .. '9']] | (i, x) <- zip [1 ..] token]
    replace a b = map (\c -> if c == a then b else c)

-- | find <dirpath> -name 'host.*.csv'
-- | grouping by '-' separator
-- | find undefined group in "policy.csv"
main :: FilePath -> IO ()
main dirpath = do
  list <- listDirectory dirpath
  let basename = filter isHostFile list
  let files = [dirpath <> "/" <> f | f <- basename]
  -- files 를 돌면서 lines <$> readFile 의 each line 에 대해 listGroup 을 hash table 에 넣고..
  -- policy.csv 의 fst, snd col 의 elem 가 존재하는지 여부를 check.
  -- files 에 대해 readFile 하면 IO 가 나오는데 이걸 map 안에서 핸들링 하는 방법을 모르겠음.

  content <- lines <$> readFile (head files)
  print content
