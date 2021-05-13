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
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty ((:|)))
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

data P = Or P P | Seq [Int] | Val Char deriving (Show)

data Q = Q1 | Q2

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: Text -> Parser Text
symbol = L.symbol space

seqParser :: HS.HashMap Int P -> [Int] -> Parser Text
seqParser rules [] = do
  return ""
seqParser rules (i : is) = do
  case HS.lookup i rules of
    Just (Or (Seq rs1) (Seq rs2)) ->
      try
        ( do
            s1 <- seqParser rules rs1
            s2 <- seqParser rules is
            return $ s1 `T.append` s2
        )
        <|> ( do
                s1 <- seqParser rules rs2
                s2 <- seqParser rules is
                return $ s1 `T.append` s2
            )
    Just (Seq rs) -> do
      s1 <- seqParser rules rs
      s2 <- seqParser rules is
      return $ s1 `T.append` s2
    Just (Val c) -> do
      c' <- MC.char c
      s2 <- seqParser rules is
      return $ T.cons c' s2
    _ -> do
      unexpected $ Label ('E' :| "rror at rule " ++ show i)

-- orParser :: HS.HashMap Int P -> [Int] -> [Int] -> Parser Text
-- orParser rules rs1 rs2 = do
--   try (seqParser rules rs1) <|> seqParser rules rs2

ruleParser :: HS.HashMap Int P -> Int -> Parser Text
ruleParser rules i = do
  case HS.lookup i rules of
    Just (Seq rs) -> do
      seqParser rules rs
    Just (Val c) -> do
      T.singleton <$> MC.char c
    _ -> do
      unexpected $ Label ('E' :| "rror at rule " ++ show i)

-- rule42 を適用できるだけ適用してから開始する
rule2Parser :: HS.HashMap Int P -> Int -> [Parser Text]
rule2Parser rules n =
  map
    ( \i ->
        do
          try $ do
            t1 <- seqParser rules [42 | _ <- [1 .. i]]
            t2 <- seqParser rules [11]
            eof
            return $ t1 `T.append` t2
    )
    [1 .. n]

solve :: Bool -> HS.HashMap Int P -> [Text] -> Int
solve isQ1 rules input =
  if isQ1
    then length $ filter (runRuleParser (ruleParser rules 0 <* eof)) input
    else length $ filter (\i -> any (`runRuleParser` i) (rule2Parser rules (length input))) input

runRuleParser :: Parser Text -> Text -> Bool
runRuleParser ruleParser input =
  case parse ruleParser "" input of
    Right _ -> True
    Left _ -> False

valRule :: Parser P
valRule = do
  symbol "\""
  c <- letterChar
  symbol "\""
  return $ Val c

seqRule :: Parser P
seqRule = do
  rs <- many $ lexeme L.decimal
  return $ Seq rs

orRule :: Parser P
orRule = do
  s1 <- seqRule
  ms2 <- optional . try $ do
    symbol "|"
    seqRule
  case ms2 of
    Just s2 -> return $ Or s1 s2
    Nothing -> return s1

bodyRule :: Parser P
bodyRule = valRule <|> orRule

rule :: Parser (Int, P)
rule = do
  n <- lexeme L.decimal
  symbol ":"
  o <- bodyRule
  return (n, o)

parseInput :: Text -> (Int, P)
parseInput input =
  case parse rule "" input of
    Right s -> s
    Left e -> error $ "PARSE ERROR: " ++ errorBundlePretty e

main :: IO ()
main = do
  contents <- lines <$> getContents
  let ruleLines = takeWhile (not . null) contents
  let inputLines = drop 1 $ dropWhile (not . null) contents
  let rules = HS.fromList . map (parseInput . T.pack) $ ruleLines
  print . solve True rules $ map T.pack inputLines
  let rules2 = HS.insert 11 (Or (Seq [42, 31]) (Seq [42, 11, 31])) $ HS.insert 8 (Or (Seq [42]) (Seq [42, 8])) rules
  print . solve False rules2 $ map T.pack inputLines
