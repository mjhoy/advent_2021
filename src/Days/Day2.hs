{-# LANGUAGE ScopedTypeVariables #-}

module Days.Day2
  ( day2_1
  ) where

import           Data.Foldable                  ( Foldable(foldl') )

type Pos = (Integer, Integer)
data Direction = Forward | Down | Up
data Command = Command Direction Integer

execute :: Pos -> Command -> Pos
execute (x, y) command = case command of
  (Command Forward x') -> (x + x', y)
  (Command Up      y') -> (x, y + y')
  (Command Down    y') -> (x, y - y')

stringToCommand :: String -> Command
stringToCommand str = Command (parseDirection fstWrd) (read sndWrd)
 where
  wds    = words str
  fstWrd = head wds
  sndWrd = wds !! 1

parseDirection :: String -> Direction
parseDirection s = case s of
  "forward" -> Forward
  "down"    -> Down
  "up"      -> Up
  d         -> error $ "unable to parse direction " ++ d

day2_1 :: FilePath -> IO String
day2_1 input = do
  ls :: [Command] <- fmap stringToCommand . lines <$> readFile input
  let (x, y) = foldl' execute (0, 0) ls
  pure $ show $ x * (-1 * y) -- negative y for depth
