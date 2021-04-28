{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad
import Control.Monad.ST
import qualified Data.HashMap.Strict as H
import Data.List (isPrefixOf)
import Data.List.Split (splitOn, startsWith)
import Data.STRef
import Debug.Trace
import qualified Text.Parsec as P
import Text.Parsec.Char
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as PT
import Data.Functor.Identity (Identity)

data Expr = Mul Expr Expr | Pls Expr Expr | Val Int deriving (Show, Eq)

primary :: Parser Expr
primary = do
  P.spaces
  b <- P.string "(" P.<|> P.many1 digit
  P.spaces
  if b == "("
    then do
      e <- expr
      P.char ')'
      P.spaces
      return e
    else
      return . Val $ read b

mul :: P.ParsecT String u Identity (Expr -> Expr -> Expr)
mul = do
  P.spaces 
  P.string "*"
  return Mul

pls :: P.ParsecT String u Identity (Expr -> Expr -> Expr)
pls = do
  P.spaces   
  P.string "+"
  return Pls


term :: Parser Expr
term = P.chainl1 primary (mul P.<|> pls)

expr:: Parser Expr
expr = term

inputParser :: Parser [Expr]
inputParser = do
  P.many1 expr

parseInput1 :: String -> [Expr]
parseInput1 s =
  case P.runParser inputParser () "parser" s of
    Right s -> s
    Left e -> error (show e ++ " --- " ++ show s)

primary2 :: Parser Expr
primary2 = do
  P.spaces
  b <- P.string "(" P.<|> P.many1 digit
  P.spaces
  if b == "("
    then do
      e <- expr2
      P.char ')'
      P.spaces
      return e
    else
      return . Val $ read b

ps :: Parser Expr
ps = P.chainl1 primary2 pls

term2 :: Parser Expr
term2 = P.chainl1 ps mul

expr2:: Parser Expr
expr2 = term2

parseInput2 :: String -> [Expr]
parseInput2 s =
  case P.runParser (P.many1 expr2) () "parser" s of
    Right s -> s
    Left e -> error (show e ++ " --- " ++ show s)

solve :: [Expr] -> Int
solve [] = 0
solve (expr:exprs) = eval expr + solve exprs
  where
    eval :: Expr -> Int
    eval (Mul x y) =
      let ex = eval x
       in ex * eval y
    eval (Pls x y) =
      let ex = eval x
       in ex + eval y
    eval (Val n) = n

main :: IO ()
main = do
  contents <- getContents  
  let exprs1 = parseInput1 contents
  print $ solve exprs1
  let exprs2 = parseInput2 contents
  print $ solve exprs2
