module Main where

import Control.Comonad
import Data.List (foldl', intercalate)
import Debug.Trace

left, right :: Z a -> Z a
left (Z (l : ls) c rs) = Z ls l (c : rs)
right (Z ls c (r : rs)) = Z (c : ls) r rs

iterate1 :: (a -> a) -> a -> [a]
iterate1 f = tail . iterate f

data Z a = Z [a] a [a]

instance Functor Z where
  fmap f (Z ls c rs) = Z (fmap f ls) (f c) (fmap f rs)

instance Comonad Z where
  extract (Z _ a _) = a
  duplicate z = Z (iterate1 left z) z (iterate1 right z)
  extend f z = Z (f <$> iterate1 left z) (f z) (f <$> iterate1 right z)

toZ :: a -> [a] -> Z a
toZ a xs = Z (repeat a) a (xs ++ repeat a)

newtype Z2 a = Z2 (Z (Z a))

instance Functor Z2 where
  fmap f (Z2 zz) = Z2 (fmap (fmap f) zz)

instance Comonad Z2 where
  extract (Z2 zz) = extract (extract zz)
  duplicate (Z2 zz) = fmap Z2 . Z2 . roll $ roll zz
    where
      roll zz = Z (iterate1 (fmap left) zz) zz (iterate1 (fmap right) zz)

newtype Z3 a = Z3 (Z (Z (Z a)))

instance Functor Z3 where
  fmap f (Z3 zzz) = Z3 (fmap (fmap (fmap f)) zzz)

instance Comonad Z3 where
  extract (Z3 zzz) = extract (extract (extract zzz))
  duplicate (Z3 zzz) = fmap Z3 . Z3 . roll . roll $ roll zzz
    where
      roll zzz = Z (iterate1 (fmap (fmap left)) zzz) zzz (iterate1 (fmap (fmap right)) zzz)

toZ2 :: a -> [[a]] -> Z2 a
toZ2 a xss = Z2 $ toZ (toZ a []) (map (toZ a) xss)

toZ3 :: a -> [[[a]]] -> Z3 a
toZ3 a xss = Z3 $ toZ (toZ (toZ a []) []) (map (toZ (toZ a []) . map (toZ a)) xss)

life :: Z3 Bool -> Bool
life z =
  let a = extract z
      n = countNeighbours z
   in (a && (n == 2 || n == 3)) || (not a && n == 3)

countNeighbours :: Z3 Bool -> Int
countNeighbours
  ( Z3
      ( Z
          ( ( Z
                (Z (t0 : _) t1 (t2 : _) : _)
                (Z (t3 : _) t4 (t5 : _))
                (Z (t6 : _) t7 (t8 : _) : _)
              )
              : _
            )
          ( Z
              (Z (n0 : _) n1 (n2 : _) : _)
              (Z (n3 : _) _ (n4 : _))
              (Z (n5 : _) n6 (n7 : _) : _)
            )
          ( ( Z
                (Z (b0 : _) b1 (b2 : _) : _)
                (Z (b3 : _) b4 (b5 : _))
                (Z (b6 : _) b7 (b8 : _) : _)
              )
              : _
            )
        )
    ) = length $ filter id [t0, t1, t2, t3, t4, t5, t6, t7, t8, n0, n1, n2, n3, n4, n5, n6, n7, b0, b1, b2, b3, b4, b5, b6, b7, b8]

cToB :: Char -> Bool
cToB '#' = True
cToB _ = False

countZ3 :: Int -> Int -> Int -> Int -> Z3 Bool -> Int
countZ3 w h z r (Z3 (Z lzzz czzz rzzz)) =
  let zzzs = take (r - 1) lzzz ++ take r [czzz] ++ take (z + r) rzzz
      zzs = map (\(Z lzz czz rzz) -> take (r - 1) lzz ++ take r [czz] ++ take (h + r) rzz) zzzs
      zs = map (map (\(Z lz cz rz) -> take (r - 1) lz ++ take r [cz] ++ take (w + r) rz)) zzs
   in foldl' (\r arrs -> r + foldl' (\r' arr -> r' + length (filter id arr)) 0 arrs) 0 zs

showZ3 :: Int -> Int -> Int -> Int -> Z3 Bool -> String
showZ3 w h z r (Z3 (Z lzzz czzz rzzz)) =
  let zzzs = take (r - 1) lzzz ++ take r [czzz] ++ take (z + r) rzzz
      zzs = map (\(Z lzz czz rzz) -> take (r - 1) lzz ++ take r [czz] ++ take (h + r) rzz) zzzs
      zs = map (map (\(Z lz cz rz) -> take (r - 1) lz ++ take r [cz] ++ take (w + r) rz)) zzs
   in foldl' (\r arrs -> r ++ "-----------------\n" ++ foldl' (\r2 arr -> r2 ++ map (\a -> if a then '#' else '.') arr ++ "\n") "" arrs ++ "\n") "" zs

solve :: Z3 Bool -> Int -> Int -> Int -> Int
solve zzz w h r =
  let zzz' = loop r zzz
   in countZ3 w h 1 r zzz'
  where
    loop :: Int -> Z3 Bool -> Z3 Bool
    loop 0 zzz = zzz
    loop n zzz = loop (n - 1) (extend life zzz)

main :: IO ()
main = do
  contents <- lines <$> getContents
  let h = length contents
  let w = length $ head contents
  let zzz = toZ3 False [map (map cToB) contents]
  -- putStr $ showZ3 w h 1 2 (extend life (extend life zzz))
  print $ solve zzz w h 6
