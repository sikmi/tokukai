{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -O2 #-}

import Control.Applicative
import Control.Arrow (second)
import Control.Monad.ST (ST, runST)
import Control.Monad.State.Strict
import qualified Data.Bits as BT
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Char (isSpace)
import Data.Coerce (coerce)
import qualified Data.HashSet as HS
import Data.List (foldl')
import Data.Maybe (fromJust, fromMaybe)
import Data.STRef.Strict (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word8)
import Debug.Trace

solve :: (Int, Int) -> Int
solve (r, c) = solve' 0 (0, 0) (r, c)
  where
    solve' :: Int -> (Int, Int) -> (Int, Int) -> Int
    solve' n (a, b) (c, d)
      | a == c && b == d = n
      | (abs (a - c) + abs (b - d)) <= 3 || (a + b) == (c + d) || (a - b) == (c - d) = n + 1
      | otherwise =
        let ab = [(a', b') | a' <- [(a - 3) .. (a + 3)], b' <- [(b - 3) .. (b + 3)], (abs (a - a') + abs (b - b')) <= 3, (a' + b') == (c + d) || (a' - b') == (c - d)]
         in if not (null ab)
              then n + 2
              else -- 上記までで解けなかったら近づく

                let (l', (x', y')) = minimum [(l, (a', b')) | a' <- [(a - 3) .. (a + 3)], b' <- [(b - 3) .. (b + 3)], let l = abs (c - a') + abs (d - b')]
                    x = if ((a < c) && (b < d)) || ((c < a) && (d < b)) then (a - b + c + d) `div` 2 else (a + b + c - d) `div` 2
                    y = if ((a < c) && (b < d)) || ((c < a) && (d < b)) then b + (x - a) else b - (x - a)
                    l = abs (c - x) + abs (d - y)
                 in if l' < 4 && l' < l
                      then solve' (n + 1) (x', y') (c, d)
                      else solve' (n + 1) (x, y) (c, d)

main :: IO ()
main = do
  (r1, c1) <- readIntTuple <$> C.getLine
  (r2, c2) <- readIntTuple <$> C.getLine
  print $ solve (abs (r1 - r2), abs (c1 - c2))

readIntTuple :: C.ByteString -> (Int, Int)
readIntTuple b = (\v -> (v U.! 0, v U.! 1)) (U.unfoldrN 2 (runParser int) b)

readIntTriple :: C.ByteString -> (Int, Int, Int)
readIntTriple b = (\v -> (v U.! 0, v U.! 1, v U.! 2)) (U.unfoldrN 3 (runParser int) b)

type Parser a = StateT C.ByteString Maybe a

runParser :: Parser a -> C.ByteString -> Maybe (a, C.ByteString)
runParser = runStateT
{-# INLINE runParser #-}

int :: Parser Int
int = coerce $ C.readInt . C.dropWhile isSpace
{-# INLINE int #-}

int1 :: Parser Int
int1 = fmap (subtract 1) int
{-# INLINE int1 #-}

char :: Parser Char
char = coerce C.uncons
{-# INLINE char #-}

byte :: Parser Word8
byte = coerce B.uncons
{-# INLINE byte #-}

skipSpaces :: Parser ()
skipSpaces = modify' (C.dropWhile isSpace)
{-# INLINE skipSpaces #-}