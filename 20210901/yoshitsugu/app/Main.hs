{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.List.Split (splitOn)
import Debug.Trace

generation :: Int -> Int
generation n
  | n == 1 = 1
  | n <= 4 = 2
  | n <= 13 = 3
  | n <= 40 = 4

generationFirstNum :: Int -> Int
generationFirstNum n = sum (map (3 ^) [0..(n - 2)]) + 1

parent :: Int -> Int
parent n
  | n == 1 = -1
  | otherwise =
    let
      g = generation n
      p = generationFirstNum (g - 1)
      f = generationFirstNum g
    in ((n - f) `div` 3) + p


solve :: (Int, Int) -> String
solve (s, d)
  | s == d = "me"
  | generation s == generation d =
      let
        ps = parent s
        pd = parent d
      in
        if ps > 0 && pd > 0
          then
            if ps == pd
              then "si"
            else
              let
                pps = parent ps
                ppd = parent pd
              in if pps > 0 && ppd > 0 && pps == ppd
                then "co"
                else "-"
        else "-"
  | generation s - 1 == generation d =
    let
      ps = parent s
      pd = parent d
      pps = if ps > 0 then parent ps else -1
    in
      if ps > 0 && ps == d
        then "mo"
      else if pps > 0 && pd > 0 && pps == pd
        then "au"
      else "-"
  | generation s == generation d - 1 =
    let
      ps = parent s
      pd = parent d
      ppd = if pd > 0 then parent pd else -1
    in
      if pd > 0 && s == pd
        then "da"
      else if ps > 0 && ppd > 0 && ps == ppd
        then "ni"
      else "-"
  | otherwise = "-"

main :: IO ()
main = do
  test "5->2" "mo"
  test "28->10" "au"
  test "1->1" "me"
  test "40->40" "me"
  test "27->27" "me"
  test "7->2" "mo"
  test "40->13" "mo"
  test "9->3" "mo"
  test "4->1" "mo"
  test "1->3" "da"
  test "12->35" "da"
  test "3->8" "da"
  test "6->19" "da"
  test "38->40" "si"
  test "9->8" "si"
  test "4->2" "si"
  test "15->16" "si"
  test "40->12" "au"
  test "10->4" "au"
  test "21->5" "au"
  test "8->2" "au"
  test "3->5" "ni"
  test "11->39" "ni"
  test "2->13" "ni"
  test "13->32" "ni"
  test "14->22" "co"
  test "40->34" "co"
  test "5->8" "co"
  test "12->10" "co"
  test "1->27" "-"
  test "8->1" "-"
  test "12->22" "-"
  test "2->40" "-"
  test "32->31" "-"
  test "13->14" "-"



test :: String -> String -> IO ()
test a b = do
  let result = solve . (\a -> (head a, a !! 1)) . map read $ splitOn "->" a
  putStrLn $ if result == b then "OK" else "NG: " ++ a ++ "\t expected " ++ b ++ "\t result " ++ result