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

data Q = Q1 | Q2

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: Text -> Parser Text
symbol = L.symbol space

brace :: ParsecT Void Text Identity a -> ParsecT Void Text Identity a
brace = between (symbol "(") (symbol ")")

binary :: Text -> (a -> a -> a) -> Operator (ParsecT Void Text Identity) a
binary name f = InfixL (f <$ symbol name)

mulOp :: Operator (ParsecT Void Text Identity) Expr
mulOp = binary "*" Mul

addOp :: Operator (ParsecT Void Text Identity) Expr
addOp = binary "+" Add

term :: Q -> Parser Expr
term q = Val <$> lexeme L.decimal <|> brace (expr q)

expr :: Q -> Parser Expr
expr Q1 = makeExprParser (term Q1) [[mulOp, addOp]] <?> "expr"
expr Q2 = makeExprParser (term Q2) [[addOp], [mulOp]] <?> "expr"

parseInput :: Q -> Text -> Expr
parseInput q input =
  case parse (expr q) "" input of
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
  let exprs1 = map (parseInput Q1 . T.pack) contents
  print $ solve exprs1
  let exprs2 = map (parseInput Q2 . T.pack) contents
  print $ solve exprs2
