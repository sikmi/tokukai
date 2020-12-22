module Main where

import Data.List

-- 一段下りるごとにどのくらい右にいくか
dx :: Int
dx = 3

-- countTree (マップ) (改変後のマップ) (x座標) (木の数)
countTree :: [String] -> [String] -> Int -> Int -> ([String], Int)
countTree [] m _ c = (m, c)
countTree (l:ls) m 0 c = countTree ls (m ++ l:[]) dx c
countTree (l:ls) m x c =
    let (l', c') = checkLine l [] x c
    in countTree ls (m ++ l':[]) (x + dx) c'

-- checkLine (行) (改変後の行) (x座標) (木の数)
checkLine :: String -> String -> Int -> Int -> (String, Int)
checkLine [] l _ c = (l, c)
checkLine ('#':chs) l 0 c = (l ++ ('O' : chs), c + 1)
checkLine (_:chs) l 0 c = (l ++ ('X' : chs), c)
checkLine (ch:chs) l x c = checkLine chs (l ++ (ch:[])) (x - 1) c

resizeMap :: Int -> [String] -> [String]
resizeMap _ [] = []
resizeMap s (l:ls) = take s (cycle l) : resizeMap s ls

main :: IO()
main = do
    input <- readFile "day3.txt"
    let ls = lines input
    -- あらかじめ必要な分だけ繰り返したマップを作っておく
    let resizedMap = resizeMap (length ls * dx) ls
    let (convertedMap, count) = countTree resizedMap [] 0 0
    writeFile "output" $ intercalate "\n" convertedMap
    putStr "count: "
    print count
