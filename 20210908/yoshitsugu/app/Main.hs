{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.List.Split (splitOn)
import Debug.Trace

rail :: Int -> Int -> [Int]
rail 1 1 = [1, 2]
rail 1 2 = [2, 3]
rail 1 3 = [3]
rail 2 1 = [1, 3]
rail 2 2 = [2]
rail 2 3 = [2, 3]
rail 3 1 = [1, 3]
rail 3 2 = [1, 2]
rail 3 3 = [3]
rail 4 1 = [1]
rail 4 2 = [1, 2]
rail 4 3 = [2, 3]
rail 5 1 = [1]
rail 5 2 = [2, 3]
rail 5 3 = [1, 3]
rail 6 1 = [1, 2]
rail 6 2 = [2]
rail 6 3 = [1, 3]
rail 7 1 = [1]
rail 7 2 = [-1]
rail 7 3 = [3]
rail 8 1 = [-1]
rail 8 2 = [2]
rail 8 3 = [3]
rail 9 1 = [1]
rail 9 2 = [2]
rail 9 3 = [-1]
rail a b = error ("invalid rail" ++ show (a, b))

solve :: [Int] -> String
solve a = 
  let result = map fst . filter (\(a, bs) -> any (/= -1) bs) . zip "abc" $ foldl nextRails [[1], [2], [3]] a
  in if null result then "-" else result

  where
    nextRails :: [[Int]] -> Int -> [[Int]]
    nextRails zs p = map (concatMap (\c -> if c == -1 then [-1] else rail p c)) zs


main :: IO ()
main = do
  test "1728398" "bc"    
  test "789" "-"    
  test "274" "ac"    
  test "185" "abc"    
  test "396" "ab"    
  test "1278" "abc"    
  test "7659832" "a"    
  test "178" "bc"    
  test "189" "ab"    
  test "197" "a"    
  test "278" "ac"    
  test "289" "bc"    
  test "297" "a"    
  test "378" "ac"    
  test "389" "b"    
  test "397" "ab"    
  test "478" "c"    
  test "489" "bc"    
  test "497" "ab"    
  test "578" "bc"    
  test "589" "b"    
  test "597" "ac"    
  test "678" "c"    
  test "689" "ab"    
  test "697" "ac"    
  test "899" "b"    
  test "7172" "ac"    
  test "54787" "bc"    
  test "83713" "bc"    
  test "149978" "-"    
  test "159735" "abc"    
  test "1449467" "abc"    
  test "9862916" "b"    
  test "96112873" "ab"    
  test "311536789" "-"    
  test "281787212994" "abc"    
  test "697535114542" "ac"

test :: String -> String -> IO ()
test a b = do
  let result = solve (map (read . (:[])) a)
  putStrLn $ if result == b then "OK" else "NG: " ++ a ++ "\t expected " ++ b ++ "\t result " ++ result
