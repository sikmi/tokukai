module Main where

import Data.List.Split (splitOn)
import Debug.Trace (trace)

toPassport :: String -> [(String, String)]
toPassport str = map toKeyValue $ splitOn " " $ map n2b str
  where
    n2b :: Char -> Char
    n2b '\n' = ' '
    n2b c = c

    toKeyValue :: String -> (String, String)
    toKeyValue s = (head kv, kv !! 1)
      where
        kv = splitOn ":" s

toPassports :: [String] -> [[(String, String)]]
toPassports = map toPassport

requiredPassportKeys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

validPassport :: [(String, String)] -> Bool
validPassport p = enoughKeys p && validValues p
  where
    enoughKeys :: [(String, String)] -> Bool
    enoughKeys p = all (\k -> k `elem` map fst p) requiredPassportKeys

    validValues :: [(String, String)] -> Bool
    validValues [] = True
    validValues (("byr", v) : rest) = 1920 <= toInt v && toInt v <= 2002 && validValues rest
    validValues (("iyr", v) : rest) = 1920 <= toInt v && toInt v <= 2020 && validValues rest
    validValues (("eyr", v) : rest) = 2020 <= toInt v && toInt v <= 2030 && validValues rest
    validValues (("hgt", v) : rest)
      | length v > 2 && v !! (length v - 2) == 'c' && v !! (length v - 1) == 'm' =
        let i = toInt $ take (length v - 2) v
         in 150 <= i && i <= 193 && validValues rest
      | length v > 2 && v !! (length v - 2) == 'i' && v !! (length v - 1) == 'n' =
        let i = toInt $ take (length v - 2) v
         in 59 <= i && i <= 76 && validValues rest
      | otherwise = False
    validValues (("hcl", v) : rest) =
      length v == 7 && head v == '#' && validHairColorFormat (tail v) && validValues rest
      where
        validHairColorFormat = all (\c -> isDigit c || c `elem` "abcdef")
    validValues (("ecl", v) : rest) =
      v `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"] && validValues rest
    validValues (("pid", v) : rest) =
      length v == 9 && all isDigit v && validValues rest
    validValues (("cid", v) : rest) = validValues rest
    validValues _ = False

    toInt :: String -> Int
    toInt = read

    isDigit :: Char -> Bool
    isDigit = flip elem "0123456789"

main :: IO ()
main = do
  input <- readFile "day4.txt"
  let passports = toPassports $ splitOn "\n\n" input
  print . length $ filter validPassport passports
