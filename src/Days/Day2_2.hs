{-# LANGUAGE ScopedTypeVariables #-}

module Days.Day2_2
  ( day2_2
  ) where

import           Data.Foldable                  ( Foldable(foldl') )

type Pos = (Integer, Integer, Integer) -- x, y, aim
data Direction = Forward | Down | Up
data Command = Command Direction Integer

execute :: Pos -> Command -> Pos
execute (x, y, aim) command = case command of
  (Command Forward x') -> (x + x', y - (aim * x'), aim)
  (Command Up      y') -> (x, y, aim - y')
  (Command Down    y') -> (x, y, aim + y')

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

day2_2 :: FilePath -> IO String
day2_2 input = do
  ls :: [Command] <- fmap stringToCommand . lines <$> readFile input
  let (x, y, aim) = foldl' execute (0, 0, 0) ls
  pure $ show $ x * (-1 * y) -- negative y for depth
