module Main where

import Data.List (foldl', intersect, nub)
import Data.List.Split (splitOn)
import Debug.Trace (trace)

countAnyMemberYes :: String -> Int
countAnyMemberYes = length . nub . filter (/= '\n')

countAllMemberYes :: String -> Int
countAllMemberYes x = length $ foldl' intersect (head $ lines x) (tail $ lines x)

main :: IO ()
main = do
  contents <- getContents
  print . sum . map countAnyMemberYes $ splitOn "\n\n" contents
  print . sum . map countAllMemberYes $ splitOn "\n\n" contents