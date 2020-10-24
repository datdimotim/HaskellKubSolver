module KubGeometry (x2MoveSlow, y2MoveSlow, z2MoveSlow) where

import KubTypes
import CubieCoord

import Prelude hiding (foldr, foldl)


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

mainRotatesToFullRotates msg rotater np | np == 0               = id 
                                        | np > 18               = error $ msg ++ " np > 18: np=" ++ (show np)
                                        | (np - 1) `mod` 3 == 2 = let t = mainRotatesToFullRotates msg rotater (np-2) in t . t
                                        | (np - 1) `mod` 3 == 1 = let t = mainRotatesToFullRotates msg rotater (np-1) in t . t . t 
                                        | (np - 1) `mod` 3 == 0 = rotater np

moveInCoordsSlow to from cubieMover p = to . map (cubieMover p) . from

x2ElemRotate :: Int -> Int -> Int
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

y2ElemRotate :: Int -> Int -> Int
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



