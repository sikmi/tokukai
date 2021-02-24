{-# LANGUAGE Strict #-}

module Main where

import qualified Data.ByteString.Char8 as BS
import Data.List (foldl', group, sort, unfoldr)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Vector.Unboxed as VU

readInt :: BS.ByteString -> Int
readInt = fromIntegral . fst . fromJust . BS.readInteger

differences :: VU.Vector Int -> Maybe (Int, VU.Vector Int)
differences v
  | VU.length v <= 1 = Nothing
  | otherwise = Just (fromIntegral $ (v VU.! 1) - (v VU.! 0), VU.tail v)

countRoute :: VU.Vector Int -> VU.Vector Int -> Int
countRoute memo as
  | VU.length memo == VU.length as = VU.last memo
  | otherwise = countRoute (VU.snoc memo $ countRoute1 memo as) as
  where
    countRoute1 :: VU.Vector Int -> VU.Vector Int -> Int
    countRoute1 memo as =
      let n = VU.length memo
          c = as VU.! n
          mf = as VU.!? (n - 1)
          ms = as VU.!? (n - 2)
          mt = as VU.!? (n - 3)
          withIndex = zip [1 ..] $ map (fromMaybe (-100)) [mf, ms, mt]
          filtered = filter (\(i, t) -> (c - t) <= 3 && (t == 0 || (fromMaybe 0 (memo VU.!? (n - i)) > 0))) withIndex
       in sum $ map (\(i, _) -> if (memo VU.! (n - i)) == 0 then 1 else memo VU.! (n - i)) filtered

main :: IO ()
main = do
  as <- VU.fromList . sort . map readInt . BS.lines <$> BS.getContents
  let vas = VU.snoc (VU.cons 0 as) (VU.last as + 3)
  let (a, b) = VU.partition (1 ==) $ VU.unfoldr differences vas
  print $ VU.length a * VU.length b
  print $ countRoute VU.empty vas
