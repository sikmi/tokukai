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
import Data.Char (isSpace, ord)
import Data.Coerce (coerce)
-- import qualified Data.HashMap.Strict as HS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (foldl', sort)
import Data.Maybe (fromJust, fromMaybe)
import Data.STRef (STRef)
import Data.STRef.Strict (modifySTRef', newSTRef, readSTRef, writeSTRef)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word8)
import Debug.Trace
import GHC.List (scanl')

fac :: (Integral a) => a -> a
fac 0 = 1
fac n = n * fac (n - 1)

combination :: Integer -> Integer -> Integer
combination n r
  | r > (n - r) = product [r + 1 .. n] `div` product [1 .. (n - r)]
  | otherwise = product [(n - r) + 1 .. n] `div` product [1 .. r]

solve :: Int -> Int -> Int -> Int -> Int -> Int -> Integer
solve r c x y d l =
  let xy = x * y
      dxy = combination (fromIntegral xy) (fromIntegral d)
      rest = xy - d
      lxy = if rest > l then combination (fromIntegral rest) (fromIntegral l) else 0
   in solve' r c x y (lxy + dxy) 0 0 (lxy + dxy)
  where
    solve' :: Int -> Int -> Int -> Int -> Integer -> Int -> Int -> Integer -> Integer
    solve' r c x y s px py ps =
      if px + x >= r
        then
          if py + y >= c
            then ps
            else solve' r c x y s 0 (py + 1) ((ps + s) `mod` 1000000007)
        else solve' r c x y s (px + 1) py ((ps + s) `mod` 1000000007)

main :: IO ()
main = do
  (r, c) <- readIntTuple <$> C.getLine
  (x, y) <- readIntTuple <$> C.getLine
  (d, l) <- readIntTuple <$> C.getLine

  print $ solve r c x y d l

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