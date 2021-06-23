{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

import Data.List (nub)
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

data Direction = E | W | SE | SW | NE | NW deriving (Show, Eq)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: Text -> Parser Text
symbol = L.symbol space

directionParser :: Parser Direction
directionParser = do
  d <- try (symbol "se" <|> symbol "sw" <|> symbol "nw" <|> symbol "ne") <|> (symbol "w" <|> symbol "e")
  case d of
    "se" -> return SE
    "sw" -> return SW
    "ne" -> return NE
    "nw" -> return NW
    "e" -> return E
    "w" -> return W
    _ -> error "invalid input"

parser :: Parser [Direction]
parser = many directionParser

parseInput :: Text -> [Direction]
parseInput input =
  case parse parser "" input of
    Right s -> s
    Left e -> error $ "PARSE ERROR: " ++ errorBundlePretty e

solve1 :: [[Direction]] -> [(Int, Int)]
solve1 dss = solve1' dss []
  where
    solve1' :: [[Direction]] -> [(Int, Int)] -> [(Int, Int)]
    solve1' [] m = m
    solve1' (ds : rss) m = solve1' rss (solve1'' ds (0, 0) m)

    solve1'' :: [Direction] -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
    solve1'' [] c blacks = if c `elem` blacks then filter (/= c) blacks else c : blacks
    solve1'' (SE : ds) (a, b) blacks = solve1'' ds (a + 1, b - 1) blacks
    solve1'' (SW : ds) (a, b) blacks = solve1'' ds (a - 1, b - 1) blacks
    solve1'' (NE : ds) (a, b) blacks = solve1'' ds (a + 1, b + 1) blacks
    solve1'' (NW : ds) (a, b) blacks = solve1'' ds (a - 1, b + 1) blacks
    solve1'' (E : ds) (a, b) blacks = solve1'' ds (a + 2, b) blacks
    solve1'' (W : ds) (a, b) blacks = solve1'' ds (a - 2, b) blacks

solve2 :: [(Int, Int)] -> Int -> [(Int, Int)]
solve2 m 0 = m
solve2 m n = solve2 (next m m []) (n - 1)
  where
    next :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
    next [] _ n = n
    next (b : bs) m n =
      let abs = (b : adjacents b)
          n' =
            nub $
              filter
                ( \ab ->
                    let aabbs = filter (`elem` m) $ adjacents ab
                     in if ab `elem` m
                          then -- 元が黒のとき
                            length aabbs == 1 || length aabbs == 2
                          else -- 元が白のとき
                            length aabbs == 2
                )
                abs
       in next bs m (nub (n ++ n'))

    adjacents :: (Int, Int) -> [(Int, Int)]
    adjacents (ta, tb) =
      [ (ta', tb')
        | ta' <- [ta - 2 .. ta + 2],
          tb' <- [tb - 1 .. tb + 1],
          ( (ta /= ta')
              || (tb /= tb')
          )
            && ( (abs (ta - ta') == 2 && abs (tb - tb') == 0)
                   || (abs (ta - ta') == 1 && abs (tb - tb') == 1)
               )
      ]

main :: IO ()
main = do
  parsedLines <- map (parseInput . T.pack) . lines <$> getContents
  let result1 = solve1 parsedLines
  print $ length result1
  print . length $ solve2 result1 100
