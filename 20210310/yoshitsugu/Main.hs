{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -O2 #-}

import Control.Applicative
import Control.Arrow (second)
import Control.Monad.ST (runST)
import Control.Monad.State.Strict
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Char (isSpace)
import Data.Coerce (coerce)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word8)
import Debug.Trace

type O = (Char, Int)

pattern N :: b -> (Char, b)
pattern N l = ('N', l)

pattern S :: b -> (Char, b)
pattern S l = ('S', l)

pattern E :: b -> (Char, b)
pattern E l = ('E', l)

pattern W :: b -> (Char, b)
pattern W l = ('W', l)

pattern L :: b -> (Char, b)
pattern L d = ('L', d)

pattern R :: b -> (Char, b)
pattern R d = ('R', d)

pattern F :: b -> (Char, b)
pattern F l = ('F', l)

{-# COMPLETE N, S, E, W, L, R, F #-}

queryO :: Parser O
queryO = (,) <$> char <*> int <* skipSpaces

solve :: U.Vector O -> Int
solve vs =
  (\(_, e, n) -> abs e + abs n) $
    U.foldl' (\(d, e, n) o -> next d e n o) ('E', 0, 0) vs
  where
    next :: Char -> Int -> Int -> O -> (Char, Int, Int)
    next d e n (N l) = (d, e, n + l)
    next d e n (S l) = (d, e, n - l)
    next d e n (E l) = (d, e + l, n)
    next d e n (W l) = (d, e - l, n)
    next cd e n (L d) = (rotate (L d) cd, e, n)
    next cd e n (R d) = (rotate (R d) cd, e, n)
    next d e n (F l) = case d of
      'N' -> next d e n (N l)
      'S' -> next d e n (S l)
      'E' -> next d e n (E l)
      'W' -> next d e n (W l)

    rotate :: O -> Char -> Char
    rotate (L d) cd = head . drop (d `div` 90 `mod` 4) . dropWhile (/= cd) $ cycle ['N', 'W', 'S', 'E']
    rotate (R d) cd = head . drop (d `div` 90 `mod` 4) . dropWhile (/= cd) $ cycle ['N', 'E', 'S', 'W']

solve2 :: U.Vector O -> Int
solve2 vs =
  (\(e, n, _, _) -> abs e + abs n) $
    U.foldl' (\(e, n, we, wn) o -> next e n we wn o) (0, 0, 10, 1) vs
  where
    next :: Int -> Int -> Int -> Int -> O -> (Int, Int, Int, Int)
    next e n we wn (N l) = (e, n, we, wn + l)
    next e n we wn (S l) = (e, n, we, wn - l)
    next e n we wn (E l) = (e, n, we + l, wn)
    next e n we wn (W l) = (e, n, we - l, wn)
    next e n we wn (L d) =
      let (we', wn') = rotate we wn (- d)
       in (e, n, we', wn')
    next e n we wn (R d) =
      let (we', wn') = rotate we wn d
       in (e, n, we', wn')
    next e n we wn (F l) = (e + we * l, n + wn * l, we, wn)

    rotate we wn d =
      let d' = (d + 360) `mod` 360
       in if
              | d' == 0 -> (we, wn)
              | d' == 90 -> (wn, - we)
              | d' == 180 -> (- we, - wn)
              | d' == 270 -> (- wn, we)
              | otherwise -> error "invalid direction"

main :: IO ()
main = do
  os <- U.unfoldr (runParser queryO) <$> B.getContents
  print $ solve os
  print $ solve2 os

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