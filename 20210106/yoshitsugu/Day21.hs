module Main where

import Data.List (isPrefixOf, sum, tails)
import Data.List.Split (splitOn)

valid :: String -> Bool
valid [] = False
valid s =
  let (condition : target : _) = splitOn ": " s
      (range : c : _) = splitOn " " condition
      (min : max : _) = splitOn "-" range
      iMin = read min :: Int
      iMax = read max :: Int
      count = sum [1 | r <- tails target, c `isPrefixOf` r]
   in iMin <= count && count <= iMax

main :: IO ()
main = do
  lines <- lines <$> getContents
  print . length $ filter valid lines
