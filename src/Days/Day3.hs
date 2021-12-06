{-# LANGUAGE ScopedTypeVariables #-}

module Days.Day3
  ( day3_1
  , day3_2
  ) where

import           Data.Foldable                  ( Foldable(foldl') )

type BitCounts = [Integer]

process :: [Integer] -> String -> [Integer]
process = zipWith count
 where
  count n '0' = n - 1
  count n '1' = n + 1
  count n x   = error $ "unknown bit " ++ [x]

binaryToInteger :: [Integer] -> Integer
binaryToInteger bits = snd $ foldl' fn (1, 0) (reverse bits)
 where
  fn (power, acc) bit | bit > 0   = (power * 2, acc + power)
                      | otherwise = (power * 2, acc)

day3_1 :: FilePath -> IO String
day3_1 input = do
  ls :: [String] <- lines <$> readFile input
  let finalCount = foldl' process (repeat 0) ls
  let gamma      = binaryToInteger finalCount
  let epsilon    = binaryToInteger (map (* (-1)) finalCount)
  pure $ show (gamma * epsilon)

filterFn :: Int -> Bool -> String -> Bool
filterFn index bit counter = bit == ((counter !! index) == '1')

searchFn :: Bool -> [String] -> Int -> [String]
searchFn _        [x] _   = [x]
searchFn isCommon ls  idx = filter (filterFn idx (filterOn ls)) ls
 where
  filterOn ls =
    let counts = foldl' process (repeat 0) ls
    in  if isCommon then (counts !! idx) >= 0 else (counts !! idx) < 0

day3_2 :: FilePath -> IO String
day3_2 input = do
  ls :: [String] <- lines <$> readFile input
  let transform  = foldl' process (repeat 0)
  let bitLength  = length $ head ls
  let finalCount = transform ls
  let og = binaryToInteger $ transform $ take 1 $ foldl' (searchFn True)
                                                         ls
                                                         [0, 1 .. bitLength] -- A little hacky. Better would be to short-circuit the fold somehow.
  let c02 = binaryToInteger $ transform $ take 1 $ foldl' (searchFn False)
                                                          ls
                                                          [0, 1 .. bitLength]
  pure $ show (og * c02)
