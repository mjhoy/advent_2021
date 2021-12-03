module Days.Day1 where
import           Data.Foldable                  ( Foldable(foldl') )

type Memo = (Integer, Maybe Integer)

day1 :: FilePath -> IO String
day1 input = do
  ls <- lines <$> readFile input
  let (res, _) = foldl' process (0, Nothing :: Maybe Integer) ls
  pure (show res)

process :: Memo -> String -> Memo
process (acc, prev) line =
  let cur = read line :: Integer
  in  case prev of
        Just n | n >= cur -> (acc, Just cur)
        Just n            -> (acc + 1, Just cur)
        Nothing           -> (acc, Just cur)
