module Main where

import Data.Char (digitToInt)
import Data.List (maximum)

waterHeight :: [Int] -> Int -> [(Int, Bool)] -> [Int]
waterHeight _ 0 r = map fst r
waterHeight input h r
  | all snd r = map fst r
  | otherwise =
    let -- 現状仮定している水位以上の高さのセルはTrueにする
        r' = map (\(i, b) -> (i, h <= i)) r
        -- Trueのやつのindexをとってくる
        ri = map snd $ filter (snd . fst) $ zip r' [0 ..]
        -- Trueのindexを両端として、挟んでいる領域があればTrueにしていく
        checkedHeights = check r' ri
     in waterHeight input (h - 1) checkedHeights
  where
    check :: [(Int, Bool)] -> [Int] -> [(Int, Bool)]
    check r' [] = r'
    check r' [e] = r'
    check r' (s : e : rs)
      | notLeakedRegion r' s e = check (confirmHeight (minimumOfEnds r' s e) s e $ zip r' [0 ..]) (e : rs)
      | otherwise = check r' (e : rs)

    -- 間に高さ0がなければ水が溜められる
    notLeakedRegion :: [(Int, Bool)] -> Int -> Int -> Bool
    notLeakedRegion r s e = all (\a -> fst a /= 0) $ take (e - s) $ drop s r

    -- 両端の低い方の高さを求める
    minimumOfEnds :: [(Int, Bool)] -> Int -> Int -> Int
    minimumOfEnds r s e = min (fst (r !! s)) (fst (r !! e))

    -- 両端の間の高さを決めていく
    confirmHeight :: Int -> Int -> Int -> [((Int, Bool), Int)] -> [(Int, Bool)]
    confirmHeight h s e r
      | (s + 1) > (e - 1) = map fst r
      | otherwise =
        map
          ( \((a, b), c) ->
              if s < c && c < e
                then (h, True)
                else (a, b)
          )
          r

-- 元の数値との差をとって水のセルの数を求める
countWater :: [Int] -> [Int] -> Int
countWater h l = sum $ zipWith (-) h l

test :: String -> String -> IO ()
test input answer = do
  let line = map digitToInt input
  let result = zip line (repeat False)
  let heights = waterHeight line (maximum line) result
  putStr $ "test " ++ input ++ " => "
  if countWater heights line == read answer
    then putStrLn $ answer ++ " OK"
    else putStrLn $ answer ++ "!! NG !!"

main :: IO ()
main = do
  test "83141310145169154671122" "24"
  test "923111128" "45"
  test "923101128" "1"
  test "903111128" "9"
  test "3" "0"
  test "31" "0"
  test "412" "1"
  test "3124" "3"
  test "11111" "0"
  test "222111" "0"
  test "335544" "0"
  test "1223455321" "0"
  test "000" "0"
  test "000100020003121" "1"
  test "1213141516171819181716151413121" "56"
  test "712131415161718191817161514131216" "117"
  test "712131405161718191817161514031216" "64"
  test "03205301204342100" "1"
  test "0912830485711120342" "18"
  test "1113241120998943327631001" "20"
  test "7688167781598943035023813337019904732" "41"
  test "2032075902729233234129146823006063388" "79"
  test "8323636570846582397534533" "44"
  test "2142555257761672319599209190604843" "41"
  test "06424633785085474133925235" "51"
  test "503144400846933212134" "21"
  test "1204706243676306476295999864" "21"
  test "050527640248767717738306306596466224" "29"
  test "5926294098216193922825" "65"
  test "655589141599534035" "29"
  test "7411279689677738" "34"
  test "268131111165754619136819109839402" "102"
