{-# LANGUAGE TupleSections #-}

module Main where

import Data.Char (isDigit)
import Data.HashMap.Strict (HashMap, empty, fromList, insert, unionWith, (!))
import qualified Data.HashMap.Strict as HM (lookup)
import qualified Data.HashSet as HS
import Data.List (foldl', nub)
import Data.List.Split (splitOn)
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

amountParser :: Parser Int
amountParser = do
  value <- P.many1 P.digit
  P.space
  return $ read value

bagParser :: Parser String
bagParser = do
  color1 <- P.many1 P.letter
  P.space
  color2 <- P.many1 P.letter
  P.space
  P.string "bag"
  P.optional $ P.string "s"
  return $ color1 ++ " " ++ color2

bagAndAmountParser :: Parser (String, Int)
bagAndAmountParser = do
  amount <- amountParser
  color <- bagParser
  return (color, amount)

noBagParser :: Parser [(String, amount)]
noBagParser = do
  P.string "no other bags"
  return []

containParser :: Parser (String, [(String, Int)])
containParser = do
  bag <- bagParser
  P.space
  _ <- P.string "contain"
  P.space
  bags <- P.choice [noBagParser, bagAndAmountParser `P.sepBy` P.string ", "]
  P.string "."
  return (bag, bags)

parseLine :: String -> (String, [(String, Int)])
parseLine s =
  case P.runParser containParser () "parser" s of
    Right s -> s
    Left e -> error (show e)

childToParentsMap :: [(String, [(String, Int)])] -> HashMap String [String]
childToParentsMap [] = empty
childToParentsMap ((k, vs) : rs) = unionWith (++) (fromList $ map ((,[k]) . fst) vs) $ childToParentsMap rs

searchOuterBag :: String -> HS.HashSet String -> HashMap String [String] -> HS.HashSet String
searchOuterBag s rs m = case HM.lookup s m of
  Just vs -> if not $ null vs then memoizedSearchOuterBag vs rs m else rs
  Nothing -> rs

memoizedSearchOuterBag :: [String] -> HS.HashSet String -> HashMap String [String] -> HS.HashSet String
memoizedSearchOuterBag [] rs m = rs
memoizedSearchOuterBag (s : ss) rs m
  | HS.member s rs = memoizedSearchOuterBag ss rs m
  | otherwise = HS.union (searchOuterBag s (HS.insert s rs) m) (memoizedSearchOuterBag ss rs m)

countInnerBag :: String -> HashMap String Int -> HashMap String [(String, Int)] -> Int
countInnerBag s n m = case HM.lookup s m of
  Just vs -> memoizedCountInnerBag vs n m
  Nothing -> 0

memoizedCountInnerBag :: [(String, Int)] -> HashMap String Int -> HashMap String [(String, Int)] -> Int
memoizedCountInnerBag [] n m = 0
memoizedCountInnerBag (v : vs) n m = case HM.lookup (fst v) n of
  Just c -> snd v + snd v * c + memoizedCountInnerBag vs n m
  Nothing ->
    let vc = countInnerBag (fst v) n m
        n' = insert (fst v) vc n
     in snd v + snd v * vc + memoizedCountInnerBag vs n' m

main :: IO ()
main = do
  contents <- getContents
  let maps = map parseLine $ lines contents
  print . length . searchOuterBag "shiny gold" HS.empty $ childToParentsMap maps
  print $ countInnerBag "shiny gold" empty $ fromList maps
