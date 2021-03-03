module Main where

import Control.Comonad
import Data.List (intercalate)

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

data Sheet = Empty | Occupied | Floor | Wall deriving (Show, Eq)

toZ2 :: a -> [[a]] -> Z2 a
toZ2 a xss = Z2 $ toZ (toZ a []) (map (toZ a) xss)

countNeighbours :: Z2 Sheet -> Int
countNeighbours
  ( Z2
      ( Z
          (Z (n0 : _) n1 (n2 : _) : _)
          (Z (n3 : _) _ (n4 : _))
          (Z (n5 : _) n6 (n7 : _) : _)
        )
    ) = length $ filter (== Occupied) [n0, n1, n2, n3, n4, n5, n6, n7]

data Direction
  = T
  | TL
  | TR
  | L
  | BL
  | B
  | BR
  | R

countFirstSheets :: Z2 Sheet -> Int
countFirstSheets z =
  countFirstSheets1 T z
    + countFirstSheets1 TL z
    + countFirstSheets1 TR z
    + countFirstSheets1 B z
    + countFirstSheets1 BL z
    + countFirstSheets1 BR z
    + countFirstSheets1 L z
    + countFirstSheets1 R z
  where
    countFirstSheets1 :: Direction -> Z2 Sheet -> Int
    countFirstSheets1 T (Z2 (Z ((Z _ Occupied _) : _) _ _)) = 1
    countFirstSheets1 T (Z2 (Z ((Z _ Empty _) : _) _ _)) = 0
    countFirstSheets1 T (Z2 (Z ((Z _ Wall _) : _) _ _)) = 0
    countFirstSheets1 T z@(Z2 (Z ((Z _ Floor _) : _) _ _)) = countFirstSheets1 T (next T z)
    countFirstSheets1 TL (Z2 (Z ((Z (Occupied : _) _ _) : _) _ _)) = 1
    countFirstSheets1 TL (Z2 (Z ((Z (Empty : _) _ _) : _) _ _)) = 0
    countFirstSheets1 TL (Z2 (Z ((Z (Wall : _) _ _) : _) _ _)) = 0
    countFirstSheets1 TL z@(Z2 (Z ((Z (Floor : _) _ _) : _) _ _)) = countFirstSheets1 TL (next TL z)
    countFirstSheets1 TR (Z2 (Z ((Z _ _ (Occupied : _)) : _) _ _)) = 1
    countFirstSheets1 TR (Z2 (Z ((Z _ _ (Empty : _)) : _) _ _)) = 0
    countFirstSheets1 TR (Z2 (Z ((Z _ _ (Wall : _)) : _) _ _)) = 0
    countFirstSheets1 TR z@(Z2 (Z ((Z _ _ (Floor : _)) : _) _ _)) = countFirstSheets1 TR (next TR z)
    countFirstSheets1 L (Z2 (Z _ (Z (Occupied : _) _ _) _)) = 1
    countFirstSheets1 L (Z2 (Z _ (Z (Empty : _) _ _) _)) = 0
    countFirstSheets1 L (Z2 (Z _ (Z (Wall : _) _ _) _)) = 0
    countFirstSheets1 L z@(Z2 (Z _ (Z (Floor : _) _ _) _)) = countFirstSheets1 L (next L z)
    countFirstSheets1 R (Z2 (Z _ (Z _ _ (Occupied : _)) _)) = 1
    countFirstSheets1 R (Z2 (Z _ (Z _ _ (Empty : _)) _)) = 0
    countFirstSheets1 R (Z2 (Z _ (Z _ _ (Wall : _)) _)) = 0
    countFirstSheets1 R z@(Z2 (Z _ (Z _ _ (Floor : _)) _)) = countFirstSheets1 R (next R z)
    countFirstSheets1 B (Z2 (Z _ _ ((Z _ Occupied _) : _))) = 1
    countFirstSheets1 B (Z2 (Z _ _ ((Z _ Empty _) : _))) = 0
    countFirstSheets1 B (Z2 (Z _ _ ((Z _ Wall _) : _))) = 0
    countFirstSheets1 B z@(Z2 (Z _ _ ((Z _ Floor _) : _))) = countFirstSheets1 B (next B z)
    countFirstSheets1 BL (Z2 (Z _ _ ((Z (Occupied : _) _ _) : _))) = 1
    countFirstSheets1 BL (Z2 (Z _ _ ((Z (Empty : _) _ _) : _))) = 0
    countFirstSheets1 BL (Z2 (Z _ _ ((Z (Wall : _) _ _) : _))) = 0
    countFirstSheets1 BL z@(Z2 (Z _ _ ((Z (Floor : _) _ _) : _))) = countFirstSheets1 BL (next BL z)
    countFirstSheets1 BR (Z2 (Z _ _ ((Z _ _ (Occupied : _)) : _))) = 1
    countFirstSheets1 BR (Z2 (Z _ _ ((Z _ _ (Empty : _)) : _))) = 0
    countFirstSheets1 BR (Z2 (Z _ _ ((Z _ _ (Wall : _)) : _))) = 0
    countFirstSheets1 BR z@(Z2 (Z _ _ ((Z _ _ (Floor : _)) : _))) = countFirstSheets1 BR (next BR z)

    next :: Direction -> Z2 Sheet -> Z2 Sheet
    next T (Z2 z) = Z2 (left z)
    next TL (Z2 z) = Z2 (fmap left (left z))
    next TR (Z2 z) = Z2 (fmap right (left z))
    next L (Z2 z) = Z2 (fmap left z)
    next R (Z2 z) = Z2 (fmap right z)
    next B (Z2 z) = Z2 (right z)
    next BL (Z2 z) = Z2 (fmap left (right z))
    next BR (Z2 z) = Z2 (fmap right (right z))

life :: Z2 Sheet -> Sheet
life z =
  case a of
    Wall -> Wall
    Floor -> Floor
    Empty -> if n == 0 then Occupied else Empty
    Occupied -> if n >= 4 then Empty else Occupied
  where
    a = extract z
    n = countNeighbours z

life2 :: Z2 Sheet -> Sheet
life2 z =
  case a of
    Wall -> Wall
    Floor -> Floor
    Empty -> if n == 0 then Occupied else Empty
    Occupied -> if n >= 5 then Empty else Occupied
  where
    a = extract z
    n = countFirstSheets z

cToS :: Char -> Sheet
cToS 'L' = Empty
cToS '#' = Occupied
cToS '.' = Floor
cToS _ = Wall

sToC :: Sheet -> Char
sToC Empty = 'L'
sToC Occupied = '#'
sToC Floor = '.'
sToC Wall = ' '

detectFixed :: Int -> Int -> (Z2 Sheet -> Sheet) -> Z2 Sheet -> Z2 Sheet
detectFixed w h f zz =
  let zz' = extend f zz
   in if fromZ2 w h zz' == fromZ2 w h zz then zz else detectFixed w h f zz'

fromZ2 :: Int -> Int -> Z2 a -> [[a]]
fromZ2 w h (Z2 (Z _ _ rows)) = map (\(Z _ _ row) -> take w row) $ take h rows

showZ2 :: Int -> Int -> Z2 Char -> IO ()
showZ2 w h (Z2 (Z _ _ rows)) = do
  flip mapM_ (take h rows) $ \(Z _ _ row) -> do
    putStrLn . unwords . map pure $ take w row

countOccupied :: [[Sheet]] -> Int
countOccupied =
  foldr (\a -> (+) (length $ filter (== Occupied) a)) 0

main :: IO ()
main =
  do
    contents <- lines <$> getContents
    let h = length contents
    let w = length $ head contents
    let zz = toZ2 Wall $ map (map cToS) contents
    print . countOccupied . fromZ2 w h $ detectFixed w h life zz
    print . countOccupied . fromZ2 w h $ detectFixed w h life2 zz
