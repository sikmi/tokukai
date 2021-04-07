{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad
import Control.Monad.ST
import qualified Data.HashMap.Strict as H
import Data.List.Split (splitOn)
import Data.STRef
import Debug.Trace
import qualified Text.Parsec as P
import Text.Parsec.Char
import Text.Parsec.String (Parser)

data ValueRange = Or ValueRange ValueRange | R Int Int deriving (Show, Eq)

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
solve1 kvs (n : ns) = sum (filter (invalidRanges kvs) n) + solve1 kvs ns
  where
    invalidRanges :: [(String, ValueRange)] -> Int -> Bool
    invalidRanges [] n = True
    invalidRanges ((_, Or (R min1 max1) (R min2 max2)) : rs) n = (n < min1 || max1 < n) && (n < min2 || max2 < n) && invalidRanges rs n
    invalidRanges ((_, R min1 max1) : rs) n = (n < min1 || max1 < n) && invalidRanges rs n

main :: IO ()
main = do
  contents <- getContents
  let (kv, y, nearby) = parseInput contents
  print $ solve1 kv nearby
