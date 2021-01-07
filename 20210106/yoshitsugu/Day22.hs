module Main where

import Data.List (isPrefixOf, sum, tails)
import Data.List.Split (splitOn)

valid :: String -> Bool
valid [] = False
valid s =
  let (condition : target : _) = splitOn ": " s
      (range : cs : _) = splitOn " " condition
      c = head cs
      (min : max : _) = splitOn "-" range
      iMin = read min :: Int
      iMax = read max :: Int
      minContain = target !! (iMin - 1) == c
      maxContain = target !! (iMax - 1) == c
   in (minContain && not maxContain) || (not minContain && maxContain)

main :: IO ()
main = do
  lines <- lines <$> getContents
  print . length $ filter valid lines
