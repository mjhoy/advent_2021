module Main where

import           Days.Day1
import           Days.Day2
import           Days.Day2_2
import           Days.Day3
import           System.Environment             ( getArgs )


main :: IO ()
main = do
  args <- getArgs
  case args of
    [day, part, inputFile] -> processDay day part inputFile
    _                      -> putStrLn "usage: [day] [part] [input file]"

processDay :: String -> String -> FilePath -> IO ()
processDay "1" "1" inputFile = day1_1 inputFile >>= putStrLn
processDay "1" "2" inputFile = day1_2 inputFile >>= putStrLn
processDay "2" "1" inputFile = day2_1 inputFile >>= putStrLn
processDay "2" "2" inputFile = day2_2 inputFile >>= putStrLn
processDay "3" "1" inputFile = day3_1 inputFile >>= putStrLn
processDay "3" "2" inputFile = day3_2 inputFile >>= putStrLn
processDay day part _ =
  putStrLn $ "unexpected day " ++ day ++ " and part " ++ part
