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
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Char (isSpace)
import Data.Coerce (coerce)
import Data.Foldable (asum)
import Data.List (scanl', sortOn)
import Data.Maybe (fromJust, isJust)
import Data.STRef
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word8)
import Debug.Trace

parseInt :: Parser Int
parseInt = do
  i <- int
  skipChar ','
  return i

parseX :: Parser Int
parseX = do
  c <- char
  if c /= 'x'
    then error "Invalid Char"
    else do
      skipChar ','
      return 0

solve :: Int -> U.Vector Int -> Int
solve n ids =
  uncurry (*) $
    U.foldl'
      ( \(r, rid) bid ->
          if
              | bid == 0 -> (r, rid)
              | bid - n `mod` bid < r -> (bid - n `mod` bid, bid)
              | otherwise -> (r, rid)
      )
      (maxBound, 0)
      ids

extgcd :: Int -> Int -> (Int, Int)
extgcd a 0 = (1, 0)
extgcd a b =
  let (c, d) = a `divMod` b
      (e, f) = extgcd b d
   in (f, e - c * f)

solve2 :: Int -> U.Vector Int -> Int
solve2 n ids =
  let ss = getOffsets ids
   in fst $
        U.foldl'
          ( \(r, m) s ->
              let (p, q) = extgcd m (fst s)
                  d = m * p + q * fst s
                  t = (snd s - r) `div` d * p `mod` (fst s `div` d)
               in if (snd s - r) `mod` d /= 0
                    then (0, -1)
                    else (r + m * t, m * fst s `div` d)
          )
          (0, 1)
          ss
  where
    getOffsets :: U.Vector Int -> U.Vector (Int, Int)
    getOffsets ids = U.create $ do
      vs <- UM.new (U.length (U.filter (/= 0) ids))
      U.foldM'_
        ( \(i, n) bid ->
            if bid /= 0
              then do
                UM.unsafeWrite vs n (bid, i)
                return (i - 1, n + 1)
              else do
                return (i - 1, n)
        )
        (0, 0)
        ids
      return vs

    f1 :: (Int, Int, Int, Int) -> Int
    f1 (a, _, _, _) = a
    f2 :: (Int, Int, Int, Int) -> Int
    f2 (_, b, _, _) = b
    f3 :: (Int, Int, Int, Int) -> Int
    f3 (_, _, c, _) = c
    f4 :: (Int, Int, Int, Int) -> Int
    f4 (_, _, _, d) = d

main :: IO ()
main = do
  n <- fst . fromJust . C.readInt <$> C.getLine
  ids <- U.unfoldr (runParser $ asum [parseInt, parseX]) <$> C.getLine
  print $ solve n ids
  print $ solve2 n ids

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

skipChar :: Char -> Parser ()
skipChar c = modify' (C.dropWhile (== c))
{-# INLINE skipChar #-}