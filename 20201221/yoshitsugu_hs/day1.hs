module Main where

mulOfSum :: Int -> [Int] -> Maybe Int
mulOfSum n [] = Nothing
mulOfSum n xs =
    let r = [(i, j) | i <- xs, j <- xs, i /= j && i + j == n]
    in case r of 
        ((f, s):_) -> Just $ f * s
        _ -> Nothing

mulOfSum3 :: Int -> [Int] -> Maybe Int
mulOfSum3 n [] = Nothing
mulOfSum3 n xs =
    let r = [(i, j, k) | 
              i <- xs,
              j <- (filter (\x -> x /= i) xs),
              k <- (filter (\x -> x /= i && x /= j) xs),
              i + j + k == n]
    in case r of 
        ((f, s, t):_) -> Just $ f * s * t
        _ -> Nothing

main :: IO()
main = do
    input <- readFile "day1.txt"
    let numbers = map (\x -> read x :: Int) $ lines input
    putStrLn . show $ mulOfSum 2020 numbers
    putStrLn. show $ mulOfSum3 2020 numbers
    