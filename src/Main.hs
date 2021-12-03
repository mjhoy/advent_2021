module Main where

import           Days.Day1                      ( day1 )
import           System.Environment             ( getArgs )

main :: IO ()
main = do
  args <- getArgs
  case args of
    [day, inputFile] -> processDay day inputFile
    _                -> putStrLn "usage: [day] [input file]"

processDay :: String -> FilePath -> IO ()
processDay "1" inputFile = day1 inputFile >>= putStrLn
processDay day _         = putStrLn $ "unexpected day " ++ day
