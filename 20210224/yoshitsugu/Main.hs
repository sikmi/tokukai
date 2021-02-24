module Main where

import qualified Data.ByteString.Char8 as BS
import Data.List (foldl', group, sort, unfoldr)
import Data.Maybe (fromJust)

readInt :: BS.ByteString -> Integer
readInt = fst . fromJust . BS.readInteger

differences :: [Integer] -> Maybe (Int, [Integer])
differences [] = Nothing
differences [a] = Nothing
differences (a : b : cs) = Just (fromIntegral (b - a), b : cs)

main :: IO ()
main = do
  as <- sort . map readInt . BS.lines <$> BS.getContents
  let (a : b : _) = map length . group . sort $ unfoldr differences ((0 : as) ++ [last as + 3])
  print $ a * b
