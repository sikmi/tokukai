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

parser :: Parser [Int]
parser = do
  symbol "Player"
  lexeme L.decimal
  symbol ":"
  ns <- many $ lexeme L.decimal
  eof
  return ns

parseInput :: Text -> [Int]
parseInput input =
  case parse parser "" input of
    Right s -> s
    Left e -> error $ "PARSE ERROR: " ++ errorBundlePretty e

solve1 :: [Int] -> [Int] -> Int
solve1 [] bs = score bs
solve1 as [] = score as
solve1 (a : as) (b : bs) =
  if a > b
    then solve1 (as ++ [a, b]) bs
    else solve1 as (bs ++ [b, a])

score :: [Int] -> Int
score cs = sum $ zipWith (*) (reverse cs) [1 ..]

solve2 :: [Int] -> [Int] -> Int
solve2 as bs = score . snd $ solve2' as bs ([], [])
  where
    solve2' :: [Int] -> [Int] -> ([[Int]], [[Int]]) -> (Bool, [Int])
    solve2' [] bs _ = (False, bs)
    solve2' as [] _ = (True, as)
    solve2' aas@(a : as) bbs@(b : bs) (pas, pbs)
      | aas `elem` pas && bbs `elem` pbs = (True, as ++ [a, b])
      | a <= length as && b <= length bs =
        if fst $ solve2' (take a as) (take b bs) ([], [])
          then solve2' (as ++ [a, b]) bs (pas ++ [aas], pbs ++ [bbs])
          else solve2' as (bs ++ [b, a]) (pas ++ [aas], pbs ++ [bbs])
      | otherwise =
        if a > b
          then solve2' (as ++ [a, b]) bs (pas ++ [aas], pbs ++ [bbs])
          else solve2' as (bs ++ [b, a]) (pas ++ [aas], pbs ++ [bbs])

main :: IO ()
main = do
  contents <- getContents
  let parsedLines = map (parseInput . T.pack) $ splitOn "\n\n" contents
  print $ solve1 (head parsedLines) (last parsedLines)
  print $ solve2 (head parsedLines) (last parsedLines)
