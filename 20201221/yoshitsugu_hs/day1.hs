module Main where

mulOfSum :: Int -> [Int] -> Maybe Int
mulOfSum n [] = Nothing
mulOfSum n xs =
    let r = [(i, j) | i <- xs, j <- xs, i /= j && i + j == n]
    in case r of 
        ((f, s):_) -> Just $ f * s
        _ -> Nothing

main :: IO()
main = do
    input <- readFile "day1.txt"
    let numbers = map (\x -> read x :: Int) $ lines input
    putStrLn . show $ mulOfSum 2020 numbers
    