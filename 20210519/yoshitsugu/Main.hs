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
import Data.List (foldl')
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

-- 回転
r :: [[a]] -> [[a]]
r [] = []
r ([] : _) = []
r m = map last m : r (map init m)

-- 反転
f :: [[a]] -> [[a]]
f [] = []
f ([] : _) = []
f m = map reverse m

-- 全てのパターンは 回転がn回(n=0..3)、反転が0,1回で表わせる
t :: [[a]] -> [[[a]]]
t a = [iterate f (iterate r a !! i) !! j | i <- [0 .. 3], j <- [0, 1]]

type P = (Int, [[Bool]])

solve1 :: [P] -> Int
solve1 n =
  let mapByPaths = groupByN $ filter sameEdge [(n1, n2) | n1 <- n, n2 <- n, n1 /= n2]
      mapByPathsList = HS.toList mapByPaths
      corners = filter (\(k, vs) -> length vs == 2) $ traceShowId mapByPathsList
   in foldl' (\r (k, _) -> r * k) 1 corners

sameEdge :: (P, P) -> Bool
sameEdge ((n1, a), (n2, b)) = or $ do
  ta <- t a
  tb <- t b
  let c = [head, map head, last, map last]
  return $ any (uncurry (==)) [(cta, ctb) | cta <- map (\c' -> c' ta) c, ctb <- map (\c' -> c' tb) c]

groupByN :: [(P, P)] -> HS.HashMap Int (S.HashSet Int)
groupByN [] = HS.empty
groupByN (((n1, _), (n2, _)) : rs) = HS.insertWith S.union n2 (S.singleton n1) . HS.insertWith S.union n1 (S.singleton n2) $ groupByN rs

seaMonster :: [[Bool]]
seaMonster =
  map
    ( map
        (== '#')
    )
    [ "                    # ",
      "#    ##    ##    ###",
      " #  #  #  #  #  #   "
    ]

solve2 :: [P] -> Int
solve2 n =
  let mapByPaths = groupByN $ filter sameEdge [(n1, n2) | n1 <- n, n2 <- n, n1 /= n2]
      mapByPathsList = HS.toList mapByPaths
      corners = filter (\(k, vs) -> length vs == 2) $ traceShowId mapByPathsList
      seaMap = getSeaMap (fst $ head corner) mapByPaths n
   in 10

sqrtInt :: Int -> Int
sqrtInt = sqrt

getSeaMap :: Int -> HS.HashMap Int (S.HashSet Int) -> [P]
getSeaMap n map ps = undefined -- do
-- forM [0 .. (sqrtInt (length p) - 1)]

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: Text -> Parser Text
symbol = L.symbol space

bparser :: Parser [Bool]
bparser = do
  cs <- some (MC.char '#' <|> MC.char '.')
  space
  return $ map (== '#') cs

parser :: Parser (Int, [[Bool]])
parser = do
  symbol "Tile"
  n <- lexeme L.decimal
  symbol ":"
  bs <- some bparser
  eof
  return (n, bs)

parseInput :: Text -> (Int, [[Bool]])
parseInput input =
  case parse parser "" input of
    Right s -> s
    Left e -> error $ "PARSE ERROR: " ++ errorBundlePretty e

main :: IO ()
main = do
  contents <- getContents
  let images = map (parseInput . T.pack) $ splitOn "\n\n" contents
  print $ solve1 images
