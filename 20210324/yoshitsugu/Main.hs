{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -O2 #-}

import Control.Applicative
import Control.Arrow (second)
import qualified Control.Monad.Primitive as P
import Control.Monad.ST (ST, runST)
import Control.Monad.State.Strict
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Char (digitToInt, isSpace)
import Data.Coerce (coerce)
import Data.Foldable (asum)
import Data.List (scanl', sortOn)
import Data.Maybe (fromJust, isJust, listToMaybe)
import Data.STRef
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word8)
import Debug.Trace
import Numeric (readInt)
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

data Command = Mask (Int, Int) | Mem (Int, Int) deriving (Show)

readBin :: Integral a => String -> Maybe a
readBin = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt

maskParser :: Parser Command
maskParser = do
  P.string "mask = "
  value <- P.many1 $ P.choice [P.char 'X', P.char '1', P.char '0']
  let orFilter = readBin $ map (\c -> if c == 'X' then '0' else c) value
  let andFilter = readBin $ map (\c -> if c == 'X' then '1' else c) value
  return $ Mask (fromJust orFilter, fromJust andFilter)

memParser :: Parser Command
memParser = do
  P.string "mem["
  addr <- P.many1 P.digit
  P.string "] = "
  value <- P.many1 P.digit
  return $ Mem (read addr, read value)

lineParser :: Parser Command
lineParser = P.try maskParser <|> memParser

parseLine :: String -> Command
parseLine s =
  case P.runParser lineParser () "parser" s of
    Right s -> s
    Left e -> error (show e ++ show s)

solve :: Int -> [Command] -> Int
solve n = solve' (U.replicate (n + 1) 0) (0, 0)
  where
    solve' :: U.Vector Int -> (Int, Int) -> [Command] -> Int
    solve' ms _ [] = U.sum ms
    solve' ms f ((Mask f') : rcs) = solve' ms f' rcs
    solve' ms f@(orFilter, andFilter) ((Mem (m, v)) : rcs) =
      let nv = andFilter .&. (orFilter .|. v)
       in solve' (U.update ms $ U.fromList [(m, nv)]) f rcs

main :: IO ()
main = do
  contents <- getContents
  let commands = map parseLine $ lines contents
  print $ solve (maxMemNum commands) commands
  where
    maxMemNum :: [Command] -> Int
    maxMemNum [] = 0
    maxMemNum ((Mask _) : bs) = maxMemNum bs
    maxMemNum ((Mem (m, _)) : bs) = max m $ maxMemNum bs
