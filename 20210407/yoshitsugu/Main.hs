{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad
import Control.Monad.ST
import qualified Data.HashMap.Strict as H
import Data.List (isPrefixOf)
import Data.List.Split (splitOn, startsWith)
import Data.STRef
import Debug.Trace
import qualified Text.Parsec as P
import Text.Parsec.Char
import Text.Parsec.String (Parser)

data ValueRange = Or ValueRange ValueRange | R Int Int deriving (Show, Eq)

include :: (String, ValueRange) -> Int -> Bool
include (_, Or (R min1 max1) (R min2 max2)) n = (min1 <= n && n <= max1) || (min2 <= n && n <= max2)
include (_, R min1 max1) n = min1 <= n && n <= max1

valueRangeParser :: Parser ValueRange
valueRangeParser = do
  min <- P.many1 P.alphaNum
  P.char '-'
  max <- P.many1 P.alphaNum
  return $ R (read min) (read max)

valueRangeOrParser :: Parser ValueRange
valueRangeOrParser = do
  range1 <- valueRangeParser
  P.string " or "
  Or range1 <$> valueRangeParser

keyAndValueRangeParser :: Parser (String, ValueRange)
keyAndValueRangeParser = do
  key <- P.many1 (P.alphaNum P.<|> P.char ' ')
  P.string ": "
  value <- valueRangeOrParser P.<|> valueRangeParser
  P.char '\n'
  return (key, value)

numbersParser :: Parser [Int]
numbersParser = do
  numbers <- P.sepEndBy1 (P.many1 P.alphaNum) (P.string ",")
  P.char '\n'
  return $ map read numbers

inputParser :: Parser ([(String, ValueRange)], [Int], [[Int]])
inputParser = do
  range <- P.many1 keyAndValueRangeParser
  P.char '\n'
  P.string "your ticket:\n"
  your <- numbersParser
  P.char '\n'
  P.string "nearby tickets:\n"
  nearby <- P.many1 numbersParser
  return (range, your, nearby)

parseInput :: String -> ([(String, ValueRange)], [Int], [[Int]])
parseInput s =
  case P.runParser inputParser () "parser" s of
    Right s -> s
    Left e -> error (show e ++ show s)

solve1 :: [(String, ValueRange)] -> [[Int]] -> Int
solve1 _ [] = 0
solve1 kvs (n : ns) = sum (filter (\n1 -> (not . any (`include` n1)) kvs) n) + solve1 kvs ns

solve2 :: [(String, ValueRange)] -> [Int] -> [[Int]] -> Int
solve2 rs y nearby = solve2' rs y (filterValid rs nearby) (candidates rs (length y))
  where
    filterValid :: [(String, ValueRange)] -> [[Int]] -> [[Int]]
    filterValid rs nearby = filter (all (\n -> any (`include` n) rs)) nearby

    candidates :: [(String, ValueRange)] -> Int -> [[(String, ValueRange)]]
    candidates rs yl = replicate yl rs

    solve2' :: [(String, ValueRange)] -> [Int] -> [[Int]] -> [[(String, ValueRange)]] -> Int
    solve2' rs ys nss cands
      | all ((== 1) . length) cands = product . map snd . filter (\(c, y) -> "departure" `isPrefixOf` c) $ zipWith (\cand y -> (fst (head cand), y)) cands ys
      | null nss = error "Cannot find valid keys"
      | otherwise =
        let ns = head nss
            rns = tail nss
            cands' = fix $ fit ns cands
         in solve2' rs ys rns cands'
      where
        fit :: [Int] -> [[(String, ValueRange)]] -> [[(String, ValueRange)]]
        fit ns candss = zipWith (filter . flip include) ns candss

        fix :: [[(String, ValueRange)]] -> [[(String, ValueRange)]]
        fix xss =
          let fixed = map head $ filter ((== 1) . length) xss
              xss' = map (\xs -> if ((== 1) . length) xs then xs else filter (`notElem` fixed) xs) xss
           in if xss == xss' then xss else fix xss'

main :: IO ()
main = do
  contents <- getContents
  let (kv, y, nearby) = parseInput contents
  print $ solve1 kv nearby
  print $ solve2 kv y nearby
