module Main where

import Data.List (sort)

seatPosition :: (Integer, Integer, Integer, Integer) -> String -> (Integer, Integer)
seatPosition (_, a, _, b) [] = (a, b)
seatPosition (a1, a2, b1, b2) ('F' : cs) = seatPosition (a1, ((a2 + 1 - a1) `div` 2) + a1 - 1, b1, b2) cs
seatPosition (a1, a2, b1, b2) ('B' : cs) = seatPosition (((a2 + 1 - a1) `div` 2) + a1, a2, b1, b2) cs
seatPosition (a1, a2, b1, b2) ('L' : cs) = seatPosition (a1, a2, b1, ((b2 + 1 - b1) `div` 2) + b1 - 1) cs
seatPosition (a1, a2, b1, b2) ('R' : cs) = seatPosition (a1, a2, ((b2 + 1 - b1) `div` 2) + b1, b2) cs
seatPosition _ _ = error "unreachable"

seatId :: (Integer, Integer) -> Integer
seatId (x, y) = x * 8 + y

mySeatId :: [Integer] -> Integer
mySeatId [] = error "not found"
mySeatId (a : b : rest)
  | a + 1 == b = mySeatId (b : rest)
  | a + 2 == b = a + 1
  | otherwise = error "invalid sequence"

main :: IO ()
main = do
  file <- getContents
  let seatIds = map (seatId . seatPosition (0, 127, 0, 7)) $ lines file
  print . maximum $ seatIds
  print . mySeatId $ sort [s | s <- seatIds, s /= maximum seatIds || s /= minimum seatIds]