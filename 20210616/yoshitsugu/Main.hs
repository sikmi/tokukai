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
import Control.Monad.ST (ST, runST)
import Data.Char (digitToInt, intToDigit)
import Data.Functor.Identity (Identity)
import qualified Data.HashMap.Strict as HS
import qualified Data.HashSet as S
import Data.List (foldl', intercalate, intersect, intersperse, partition, scanl', sort, (\\))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Void (Void)
import Debug.Trace

elemTriple :: Int -> (Int, Int, Int) -> Bool
elemTriple a (p1, p2, p3) = a == p1 || a == p2 || a == p3

game :: Int -> [Int] -> Int -> [Int]
game 0 a _ = a
game n (c : p1 : p2 : p3 : r) m =
  let d = destination (c - 1) (p1, p2, p3) r
      (rf : rs : _) = splitOn [d] r
   in game (n - 1) (rf ++ [d, p1, p2, p3] ++ rs ++ [c]) m
  where
    destination :: Int -> (Int, Int, Int) -> [Int] -> Int
    destination 0 p a = destination m p a
    destination n p a =
      if not (elemTriple n p) && n `elem` a then n else destination (n - 1) p a
game _ _ _ = error "invalid"

solve1 :: [Int] -> String
solve1 a =
  let (rf : rs : _) = splitOn [1] $ game 100 a (maximum a)
   in map intToDigit $ rs ++ rf

toInput2 :: Int -> Int -> [Int] -> (U.Vector Int, Int)
toInput2 m n a =
  let list = a ++ take n [m ..]
      u = U.create $ do
        v <- UM.new $ length list + 1
        connect v list
        UM.unsafeWrite v (last list) (head list)
        return v
   in (u, head list)
  where
    connect :: UM.MVector s Int -> [Int] -> ST s ()
    connect v [] = return ()
    connect v [a] = return ()
    connect v (a : b : c) = do
      UM.unsafeWrite v a b
      connect v (b : c)

solve2 :: U.Vector Int -> Int -> Int -> Int -> Int
solve2 a current round maxNum = runST $ do
  am <- U.thaw a
  cref <- newSTRef current
  forM_ [1 .. round] $ \_ -> do
    c <- readSTRef cref
    p1 <- UM.unsafeRead am c
    p2 <- UM.unsafeRead am p1
    p3 <- UM.unsafeRead am p2
    next <- UM.unsafeRead am p3
    let d = destination (c, p1, p2, p3) (c - 1)
    e <- UM.unsafeRead am d
    UM.unsafeWrite am d p1
    UM.unsafeWrite am c next
    UM.unsafeWrite am p3 e
    -- showVM am c
    writeSTRef cref next
  af <- U.unsafeFreeze am
  o1 <- UM.unsafeRead am 1
  o2 <- UM.unsafeRead am o1
  -- showVM am 1
  return $ o1 * o2
  where
    destination :: (Int, Int, Int, Int) -> Int -> Int
    destination (current, p1, p2, p3) d
      | d <= 0 = destination (current, p1, p2, p3) maxNum
      | d == p1 || d == p2 || d == p3 =
        if (d - 1) == 0
          then destination (current, p1, p2, p3) maxNum
          else destination (current, p1, p2, p3) (d - 1)
      | otherwise = d
    showVM :: UM.MVector s Int -> Int -> ST s ()
    showVM am n = do
      af <- U.unsafeFreeze am
      traceShowM $ scanl' (\b _ -> af U.! b) n [1 .. 8]
      return ()

main :: IO ()
main = do
  input <- map digitToInt <$> getLine
  putStrLn $ solve1 input
  let cupSize = 1000000
  let round = 10000000
  let (input2, start) = toInput2 (maximum input + 1) (cupSize - length input) input
  print $ solve2 input2 start round cupSize
