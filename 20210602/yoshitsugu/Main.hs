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
import Data.List (foldl', intersect, (\\))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.Split (splitOn)
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
  let m = solve1' lines HS.empty
   in countNotAllergens (concatMap fst lines) (map head $ HS.elems m)
  where
    solve1' :: [([String], [String])] -> HS.HashMap String [String] -> HS.HashMap String [String]
    solve1' [] m = solve1Extra m
    solve1' o@((is, as) : rs) m =
      let fixedCount = length $ HS.keys $ HS.filter (\v -> length v == 1) m
          h = solve1l is as m
          fixedCount2 = length $ HS.keys $ HS.filter (\v -> length v == 1) m
       in if fixedCount /= fixedCount2
            then solve1' o h
            else solve1' rs h

    solve1l :: [String] -> [String] -> HS.HashMap String [String] -> HS.HashMap String [String]
    solve1l _ [] m = m
    solve1l is (a : as) m =
      case HS.lookup a m of
        Just [b] -> solve1l is as m
        _ ->
          let fixedValues = map head $ HS.elems $ HS.filter (\v -> length v == 1) m
              nis = is \\ fixedValues
           in solve1l is as $ HS.insertWith intersect a nis m

    solve1Extra :: HS.HashMap String [String] -> HS.HashMap String [String]
    solve1Extra m =
      let unfixeds = HS.filter (\v -> length v /= 1) m
          unfixedKeys = HS.keys unfixeds
       in if null unfixedKeys
            then m
            else sve2 unfixedKeys m
      where
        sve2 :: [String] -> HS.HashMap String [String] -> HS.HashMap String [String]
        sve2 [] m = solve1Extra m
        sve2 (k : ks) m =
          let fixedValues = map head $ HS.elems $ HS.filter (\v -> length v == 1) m
           in sve2 ks (HS.update (\vs -> Just (vs \\ fixedValues)) k m)

    countNotAllergens :: [String] -> [String] -> Int
    countNotAllergens ls as = length $ filter (`notElem` as) ls

main :: IO ()
main = do
  contents <- getContents
  let parsedLines = map (parseInput . T.pack) . filter (not . null) $ splitOn "\n" contents
  print $ solve1 parsedLines
