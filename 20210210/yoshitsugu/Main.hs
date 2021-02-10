{-# LANGUAGE TupleSections #-}

module Main where

import Data.List (elem)
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

data Command = Acc | Jmp | Nop

data Value = Plus Int | Minus Int

commandParser :: Parser Command
commandParser = do
  command <- P.choice [P.string "acc", P.string "jmp", P.string "nop"]
  case command of
    "acc" -> return Acc
    "jmp" -> return Jmp
    "nop" -> return Nop
    _ -> error "Invalid operation"

valueParser :: Parser Value
valueParser = do
  unary <- P.choice [P.string "+", P.string "-"]
  value <- P.many1 P.digit
  case unary of
    "+" -> return . Plus $ read value
    "-" -> return . Minus $ read value
    _ -> error "Invalid value"

lineParser :: Parser (Command, Value)
lineParser = do
  command <- commandParser
  P.space
  value <- valueParser
  return (command, value)

parseLine :: String -> (Command, Value)
parseLine s =
  case P.runParser lineParser () "parser" s of
    Right s -> s
    Left e -> error (show e)

checkInfinitLoop :: Int -> Int -> [Int] -> [(Command, Value)] -> (Int, Bool)
checkInfinitLoop lno a lnos commands
  | lno < 0 || lno >= length commands = (a, True)
  | lno `elem` lnos = (a, False)
  | otherwise =
    case commands !! lno of
      (Nop, _) -> checkInfinitLoop (lno + 1) a (lno : lnos) commands
      (Acc, Plus n) -> checkInfinitLoop (lno + 1) (a + n) (lno : lnos) commands
      (Acc, Minus n) -> checkInfinitLoop (lno + 1) (a - n) (lno : lnos) commands
      (Jmp, Plus n) -> checkInfinitLoop (lno + n) a (lno : lnos) commands
      (Jmp, Minus n) -> checkInfinitLoop (lno - n) a (lno : lnos) commands

fixInfinitLoop :: Int -> [(Command, Value)] -> Int
fixInfinitLoop 0 _ = 0
fixInfinitLoop i commands =
  case commands !! (i - 1) of
    (Nop, v) ->
      let checked = check $ replaceNthCommandWith (i - 1) (Jmp, v) commands
       in if snd checked then fst checked else fixInfinitLoop (i - 1) commands
    (Jmp, v) ->
      let checked = check $ replaceNthCommandWith (i - 1) (Nop, v) commands
       in if snd checked then fst checked else fixInfinitLoop (i - 1) commands
    _ -> fixInfinitLoop (i - 1) commands

check :: [(Command, Value)] -> (Int, Bool)
check = checkInfinitLoop 0 0 []

replaceNthCommandWith :: Int -> (Command, Value) -> [(Command, Value)] -> [(Command, Value)]
replaceNthCommandWith n c cs =
  take n cs ++ [c] ++ drop (n + 1) cs

main :: IO ()
main = do
  contents <- getContents
  let commands = map parseLine $ lines contents
  print . fst $ checkInfinitLoop 0 0 [] commands
  print $ fixInfinitLoop (length commands) commands
