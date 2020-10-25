module KubGeometry (x2MoveSlow, y2MoveSlow, z2MoveSlow, x1MoveSlow, y1MoveSlow) where

import KubTypes
import CubieCoord

import Prelude hiding (foldr, foldl)
import Data.List


rotate a b c d x | x == a    = d
                 | x == b    = a
                 | x == c    = b
                 | x == d    = c
                 | otherwise = x


downSide = 1
frontSide = 4
leftSide = 7
rightSide = 10
backSide = 13
upSide = 16
         
{-           _________
            /2 /  /3 /|
           /__/__/__/3| 
          /  /  /  /| |
         /__/__/__/ |/|
        /1 /  /0 /| | |
       /__/__/__/0|/|/| 
       | 1|  |0 | | |7/ 
       |__|__|__|/|/|/ 
       |  |  |  | | /
       |__|__|__|/|/
       |5 |  |4 |4/ 
       |__|__|__|/    
-}

mainRotatesToFullRotates :: String -> (Povorot -> Int -> Int) -> Povorot -> Int -> Int
mainRotatesToFullRotates msg rotater np | np == 0               = id 
                                        | np > 18               = error $ msg ++ " np > 18: np=" ++ show np
                                        | (np - 1) `mod` 3 == 2 = let t = mainRotatesToFullRotates msg rotater (np-2) in t . t
                                        | (np - 1) `mod` 3 == 1 = let t = mainRotatesToFullRotates msg rotater (np-1) in t . t . t 
                                        | (np - 1) `mod` 3 == 0 = rotater np

moveInCoordsSlow :: ([Int] -> Pos) -> (Pos -> [Int]) -> (Povorot -> Int -> Int) -> Povorot -> Pos -> Pos
moveInCoordsSlow to from cubieMover p = to . map (cubieMover p) . from

x2ElemRotate :: Povorot -> Int -> Int
x2ElemRotate np | np == downSide  = rotate 5 4 7 6
                | np == frontSide = rotate 0 4 5 1
                | np == leftSide  = rotate 2 1 5 6
                | np == rightSide = rotate 4 0 3 7
                | np == backSide  = rotate 7 3 2 6
                | np == upSide    = rotate 0 1 2 3


x2MoveSlow :: Povorot -> Pos -> Pos
x2MoveSlow = moveInCoordsSlow toX2 fromX2 (mainRotatesToFullRotates "x2 mover" x2ElemRotate)


{-           _________
            /  /2 /  /|
           /__/__/__/ | 
          /1 /  /3 /| |
         /__/__/__/ |/|
        /  /0 /  /|3|11|
       /__/ _/__/ |/|/| 
       |  |0 |  | | | / 
       |__|__|__|/|/|/ 
       |9 |  |8 |8|7/
       |__|__|__|/|/
       |  |4 |  | / 
       |__|__|__|/    
-}

y2ElemRotate :: Povorot -> Int -> Int
y2ElemRotate np | np == downSide  = rotate 4  7  6  5
                | np == frontSide = rotate 4  9  0  8
                | np == leftSide  = rotate 1  9  5  10
                | np == rightSide = rotate 3  11 7  8
                | np == backSide  = rotate 2  10 6  11
                | np == upSide    = rotate 0  1  2  3


-- Т.к. fromY2 выдает список из 8 элементов, т.е. верхние и нижние ребра,
-- А mainRotatesToFullRotates для поворотов на 180 градусов должна использовать все 12 ребер, 
-- то после fromX2 нужно добавить 4 ребра, а перед toX2 их убрать
-- Поскольку использются повороты из 2 фазы, эти 4 ребра останутся в среднем поясе
y2MoveSlow :: Povorot -> Pos -> Pos
y2MoveSlow = let 
                extend = (++ [8, 9, 10, 11])
                proj   = take 8
             in 
                moveInCoordsSlow (toY2 . proj) (extend . fromY2) (mainRotatesToFullRotates "y2 mover" y2ElemRotate)


z2MoveSlow :: Povorot -> Pos -> Pos
z2MoveSlow = let 
                extend = ([0..7] ++) . map (+8)
                proj   = map (\x -> x - 8) . drop 8
             in 
                moveInCoordsSlow (toZ2 . proj) (extend . fromZ2) (mainRotatesToFullRotates "z2 mover" y2ElemRotate)


-- ===============================================================================

{-           _________
            /2 /  /3 /|
           /__/__/__/3|
          /  /  /  /| |
         /__/__/__/ |/|
        /1 /  /0 /| | |
       /__/__/__/0|/|/|
       | 1|  |0 | | |7/
       |__|__|__|/|/|/
       |  |  |  | | /
       |__|__|__|/|/
       |5 |  |4 |4/
       |__|__|__|/
-}

rotateOr (a, doa) (b, dob) (c, doc) (d, dod) (x, ox) | x == a    = (d, ox + dod)
                                                     | x == b    = (a, ox + doa)
                                                     | x == c    = (b, ox + dob)
                                                     | x == d    = (c, ox + doc)
                                                     | otherwise = (x, ox)

x1ElemRotate :: Povorot -> (Int, Int) -> (Int, Int)
x1ElemRotate np p = let
                      cycles np | np == downSide  = rotateOr (5, 0) (4, 0) (7, 0) (6, 0)
                                | np == frontSide = rotateOr (5, 2) (1, 1) (0, 2) (4, 1)
                                | np == leftSide  = rotateOr (2, 1) (1, 2) (5, 1) (6, 2)
                                | np == rightSide = rotateOr (4, 2) (0, 1) (3, 2) (7, 1)
                                | np == backSide  = rotateOr (7, 2) (3, 1) (2, 2) (6, 1)
                                | np == upSide    = rotateOr (0, 0) (1, 0) (2, 0) (3, 0)
                    in
                      case cycles np p of (n, orient) -> (n, orient `mod` 3)

x1MoveCubie :: Povorot -> [Int] -> [Int]
x1MoveCubie np = map snd . sortOn fst . map (x1ElemRotate np) . zip [0..]

x1MoveSlow :: Povorot -> Pos -> Pos
x1MoveSlow = mainRotatesToFullRotates "" $ \np -> toX1 . x1MoveCubie np . fromX1


{-           _________
            /  /2 /  /|
           /__/__/__/ | 
          /1 /  /3 /| |
         /__/__/__/ |/|
        /  /0 /  /|3|11|
       /__/ _/__/ |/|/| 
       |  |0 |  | | | / 
       |__|__|__|/|/|/ 
       |9 |  |8 |8|7/
       |__|__|__|/|/
       |  |4 |  | / 
       |__|__|__|/    
-}

y1ElemRotate :: Povorot -> (Int, Int) -> (Int, Int)
y1ElemRotate np p = let
                      cycles np | np == downSide  = rotateOr (4, 0) (7, 0) (6, 0) (5, 0)
                                | np == frontSide = rotateOr (4, 1) (9, 1) (0, 1) (8, 1)
                                | np == leftSide  = rotateOr (1, 0) (9, 0) (5, 0) (10, 0)
                                | np == rightSide = rotateOr (8, 0) (3, 0) (11, 0) (7, 0)
                                | np == backSide  = rotateOr (11, 1) (2, 1) (10, 1) (6, 1)
                                | np == upSide    = rotateOr (0, 0) (1, 0) (2, 0) (3, 0)
                    in
                      case cycles np p of (n, orient) -> (n, orient `mod` 2)
                      
y1MoveCubie :: Povorot -> [Int] -> [Int]
y1MoveCubie np = map snd . sortOn fst . map (y1ElemRotate np) . zip [0..]

y1MoveSlow :: Povorot -> Pos -> Pos
y1MoveSlow = mainRotatesToFullRotates "" $ \np -> toY1 . y1MoveCubie np . fromY1