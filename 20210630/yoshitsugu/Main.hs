{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

import Data.Either (fromRight)
import Data.List (foldl', scanl)
import Debug.Trace

loopStep :: Int -> Int -> Int
loopStep val number = (val * number) `mod` 20201227

checkLoop :: Int -> Int -> Int
checkLoop i c = snd . head . dropWhile (\(r, _) -> r /= c) $ scanl (\(r, _) j -> (loopStep r i, j)) (1, 0) [1 ..]

loop :: Int -> Int -> Int
loop c n = foldl' (\r _ -> loopStep r c) 1 [1 .. n]

solve1 :: Int -> Int -> Int
solve1 c1 c2 = loop c2 $ checkLoop 7 c1

main :: IO ()
main = do
  card1 <- (read @Int) <$> getLine
  card2 <- (read @Int) <$> getLine
  print $ solve1 card1 card2
