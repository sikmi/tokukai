{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Debug.Trace
import Text.Megaparsec hiding (State)
import qualified Text.Megaparsec as M
import Text.Megaparsec.Char hiding (char)
import qualified Text.Megaparsec.Char.Lexer as L

data Expr = Mul Expr Expr | Add Expr Expr | Val Int deriving (Show, Eq)

type Parser = Parsec Void Text

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

primary :: Int -> Parser Expr
primary n =
  do
    number <- lexeme L.decimal
    return $ Val number
    <|> between (symbol "(") (symbol ")") (expr n)

chainl1 :: Parser Expr -> Parser (Expr -> Expr -> Expr) -> Parser Expr
chainl1 p op = do
  x <- p
  rest x
  where
    rest x =
      do
        f <- op
        y <- p
        rest (f x y)
        <|> return x

muladd :: Int -> Parser Expr
muladd n = chainl1 (primary n) $ (Mul <$ symbol "*") <|> (Add <$ symbol "+")

mul :: Int -> Parser Expr
mul n = chainl1 (add n) (Mul <$ symbol "*")

add :: Int -> Parser Expr
add n = chainl1 (primary n) (Add <$ symbol "+")

term :: Int -> Parser Expr
term n = if n == 1 then muladd 1 else mul 2

expr :: Int -> Parser Expr
expr = term

parseInput :: Int -> Text -> Expr
parseInput n input =
  case parse (expr n) "" input of
    Right s -> s
    Left e -> error $ "PARSE ERROR: " ++ errorBundlePretty e

solve :: [Expr] -> Int
solve [] = 0
solve (expr : exprs) = eval expr + solve exprs
  where
    eval :: Expr -> Int
    eval (Mul x y) = eval x * eval y
    eval (Add x y) = eval x + eval y
    eval (Val n) = n

main :: IO ()
main = do
  contents <- lines <$> getContents
  let exprs1 = map (parseInput 1 . T.pack) contents
  print $ solve exprs1
  let exprs2 = map (parseInput 2 . T.pack) contents
  print $ solve exprs2
