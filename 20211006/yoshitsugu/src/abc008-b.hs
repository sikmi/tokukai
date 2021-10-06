{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -O2 #-}

import Data.Map (fromListWith, toList)
import Data.List (minimumBy)

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

main :: IO ()
main = do
  cs <- lines <$> getContents
  putStrLn . fst . minimumBy (\(_, a) (_, b) -> compare b a) . frequency $ tail cs
