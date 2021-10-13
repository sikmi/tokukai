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
import qualified Data.List as L
import Data.Maybe (fromJust, fromMaybe)
import Data.STRef (STRef)
import Data.STRef.Strict (modifySTRef', newSTRef, readSTRef, writeSTRef)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word8)
import Debug.Trace

solve :: String -> Int -> String
solve [] k = []
solve ss k = minimum ([ps | ps <- L.permutations ss , (length . filter id $ zipWith (/=) ps ss) <= k])

diffString :: String -> String ->  Int
diffString a b = length . filter id $ zipWith (/=) a b

solve2 :: String -> Int -> String
solve2 a b = undefined
-- solve2 [] k = []
-- solve2 ss k = solve2' ss k (L.sort ss) []
--   where
--     solve2' :: String -> Int -> String -> String
--     solve2' [] _ _ res = res
--     solve2' _ [] _ res = res
--     solve2' (a:as) (b:bs) k cs
--       | a == b = solve2' as bs k (cs ++ [a]) 
--       | diffString as bs <= k = solve2' as bs (k - diffString as bs) (cs ++ [a])
--       | otherwise = solve2' (a:as) 
      

main :: IO ()
main = do
  (n, k) <- readIntTuple <$> C.getLine
  s <- getLine
  putStrLn $ solve2 s k

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
{-# INLINE skipSpaces #-}{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TupleSections #-}
