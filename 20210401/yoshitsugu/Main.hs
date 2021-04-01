{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad
import Control.Monad.ST
import qualified Data.HashMap.Strict as H
import Data.List.Split (splitOn)
import Data.STRef
import Debug.Trace

solve :: Int -> [Int] -> Int
solve j ns = runST $ do
  mref <- newSTRef H.empty
  forM_ (zip ns [1 ..]) $ \(n, i) -> do
    modifySTRef' mref (H.insert n i)
  loop j (length ns) (last ns) mref
  where
    loop :: Int -> Int -> Int -> STRef s (H.HashMap Int Int) -> ST s Int
    loop j i n mref
      | i == j = return n
      | otherwise = do
        ms <- readSTRef mref
        case H.lookup n ms of
          Just pi -> do
            writeSTRef mref $ H.insert n i ms
            loop j (i + 1) (i - pi) mref
          Nothing -> do
            writeSTRef mref $ H.insert n i ms
            loop j (i + 1) 0 mref

main :: IO ()
main = do
  line <- getLine
  print . solve 2020 . map (read @Int) $ splitOn "," line
  print . solve 30000000 . map (read @Int) $ splitOn "," line