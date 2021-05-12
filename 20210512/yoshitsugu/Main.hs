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
      do
        try $ do
          t1 <- seqParser rules rs1
          t2 <- seqParser rules is
          return $ t1 `T.append` ("( r" `T.append` T.pack (show i) `T.append` ", " `T.append` t2 `T.append` " )")
        <|> ( do
                t1 <- seqParser rules rs2
                t2 <- seqParser rules is
                return $ t1 `T.append` ("( r" `T.append` T.pack (show i) `T.append` ", " `T.append` t2 `T.append` " )")
            )
    _ -> do
      t1 <- ruleParser rules i
      t2 <- seqParser rules is
      return $ t1 `T.append` ("( r" `T.append` T.pack (show i) `T.append` ", " `T.append` t2 `T.append` " )")

orParser :: HS.HashMap Int P -> [Int] -> [Int] -> Parser Text
orParser rules rs1 rs2 = do
  try (seqParser rules rs1) <|> seqParser rules rs2

ruleParser :: HS.HashMap Int P -> Int -> Parser Text
ruleParser rules i = do
  case HS.lookup i rules of
    Just (Or (Seq rs1) (Seq rs2)) -> do
      orParser rules rs1 rs2
    Just (Or _ _) -> do
      MC.char '@' -- 何もmatchしないダミー
      return ""
    Just (Seq rs) -> do
      seqParser rules rs
    Just (Val c) -> do
      c' <- MC.char c
      return $ T.singleton c'

solve :: HS.HashMap Int P -> [Text] -> Int
solve rules input = length $ filter (runRuleParser (ruleParser rules 0 <* eof)) input

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
  print . solve rules $ map T.pack inputLines
  let rules2 = HS.insert 11 (Or (Seq [42, 31]) (Seq [42, 11, 31])) $ HS.insert 8 (Or (Seq [42]) (Seq [42, 8])) rules
  print . solve rules2 $ map T.pack inputLines
