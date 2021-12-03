module Days.Day1 where

import           Data.Foldable                  ( Foldable(foldl') )

type Memo = (Integer, Maybe Integer)

day1_1 :: FilePath -> IO String
day1_1 input = do
  ls <- fmap read . lines <$> readFile input
  let (res, _) = foldl' process (0, Nothing :: Maybe Integer) ls
  pure (show res)

process :: Memo -> Integer -> Memo
process (acc, prev) cur = case prev of
  Just n | n >= cur -> (acc, Just cur)
  Just n            -> (acc + 1, Just cur)
  Nothing           -> (acc, Just cur)
