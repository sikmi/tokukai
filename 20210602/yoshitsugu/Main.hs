{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Functor.Identity (Identity)
import qualified Data.HashMap.Strict as HS
import qualified Data.HashSet as S
import Data.List (foldl', intercalate, intersect, intersperse, sort, (\\))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Debug.Trace
import Text.Megaparsec hiding (State)
import qualified Text.Megaparsec as M
import Text.Megaparsec.Char hiding (char)
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error

type Parser = Parsec Void Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: Text -> Parser Text
symbol = L.symbol space

parser :: Parser ([String], [String])
parser = do
  ingredients <- many MC.letterChar `sepBy` symbol " "
  symbol "(contains"
  allergens <- many MC.letterChar `sepBy` symbol ", "
  symbol ")"
  eof
  return (filter (not . null) ingredients, allergens)

parseInput :: Text -> ([String], [String])
parseInput input =
  case parse parser "" input of
    Right s -> s
    Left e -> error $ "PARSE ERROR: " ++ errorBundlePretty e

solve1 :: [([String], [String])] -> Int
solve1 lines =
  let m = candidates lines HS.empty
   in countNotAllergens (concatMap fst lines) (map head $ HS.elems m)

candidates :: [([String], [String])] -> HS.HashMap String [String] -> HS.HashMap String [String]
candidates [] m = finalRound m
candidates o@((is, as) : rs) m =
  let fixedCount = length $ HS.keys $ HS.filter (\v -> length v == 1) m
      h = getCandidates is as m
      fixedCount2 = length $ HS.keys $ HS.filter (\v -> length v == 1) m
   in if fixedCount /= fixedCount2
        then candidates o h
        else candidates rs h

getCandidates :: [String] -> [String] -> HS.HashMap String [String] -> HS.HashMap String [String]
getCandidates _ [] m = m
getCandidates is (a : as) m =
  case HS.lookup a m of
    Just [b] -> getCandidates is as m
    _ ->
      let fixedValues = map head $ HS.elems $ HS.filter (\v -> length v == 1) m
          nis = is \\ fixedValues
       in getCandidates is as $ HS.insertWith intersect a nis m

finalRound :: HS.HashMap String [String] -> HS.HashMap String [String]
finalRound m =
  let unfixeds = HS.filter (\v -> length v /= 1) m
      unfixedKeys = HS.keys unfixeds
   in if null unfixedKeys
        then m
        else sve2 unfixedKeys m
  where
    sve2 :: [String] -> HS.HashMap String [String] -> HS.HashMap String [String]
    sve2 [] m = finalRound m
    sve2 (k : ks) m =
      let fixedValues = map head $ HS.elems $ HS.filter (\v -> length v == 1) m
       in sve2 ks (HS.update (\vs -> Just (vs \\ fixedValues)) k m)

countNotAllergens :: [String] -> [String] -> Int
countNotAllergens ls as = length $ filter (`notElem` as) ls

solve2 :: [([String], [String])] -> String
solve2 lines =
  let m = candidates lines HS.empty
      sortedKeys = sort $ HS.keys m
   in intercalate "," $ map (\k -> head . fromJust $ HS.lookup k m) sortedKeys

main :: IO ()
main = do
  contents <- getContents
  let parsedLines = map (parseInput . T.pack) . filter (not . null) $ splitOn "\n" contents
  print $ solve1 parsedLines
  putStrLn $ solve2 parsedLines
