module Main where

import Data.List (elem, maximum, minimum, sum)

stringToInteger :: String -> Integer
stringToInteger = read

detectInvalidNumber :: [Integer] -> [Integer] -> Maybe Integer
detectInvalidNumber _ [] = Nothing
detectInvalidNumber [] _ = Nothing
detectInvalidNumber ns@(fn : tn) (b : bs)
  | b `elem` [ns !! i + ns !! j | i <- [0 .. preambleSize - 1], j <- [0 .. preambleSize - 1], i /= j] = detectInvalidNumber (tn ++ [b]) bs
  | otherwise = Just b

preambleSize :: Int
preambleSize = 25

encryptedNumber :: Integer -> Int -> Int -> [Integer] -> Maybe Integer
encryptedNumber g s e ls
  | s >= length ls = Nothing
  | e >= length ls = encryptedNumber g (s + 1) (s + 2) ls
  | otherwise =
    let sls = take (e - s) $ drop s ls
     in if g == sum sls
          then Just $ minimum sls + maximum sls
          else
            if g < sum sls
              then encryptedNumber g (s + 1) (s + 2) ls
              else encryptedNumber g s (e + 1) ls

main :: IO ()
main = do
  contents <- getContents
  let ls = map stringToInteger $ lines contents
  let a = detectInvalidNumber (take preambleSize ls) (drop preambleSize ls)
  print a
  case a of
    Just n -> print $ encryptedNumber n 0 1 ls
    Nothing -> putStrLn "Nothing"
