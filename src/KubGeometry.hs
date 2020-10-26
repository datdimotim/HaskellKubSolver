{-# LANGUAGE TupleSections #-}

module KubGeometry (x2MoveSlow, y2MoveSlow, z2MoveSlow, x1MoveSlow, y1MoveSlow) where

import KubTypes
import CubieCoord

import Prelude hiding (foldr, foldl)
import Data.List hiding (permutations)


downSide = 1
frontSide = 4
leftSide = 7
rightSide = 10
backSide = 13
upSide = 16


rotateOr (a, doa) (b, dob) (c, doc) (d, dod) (x, ox) | x == a    = (d, ox + dod)
                                                     | x == b    = (a, ox + doa)
                                                     | x == c    = (b, ox + dob)
                                                     | x == d    = (c, ox + doc)
                                                     | otherwise = (x, ox)
                                                     
                                                     
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

ugolCubieRotate :: Povorot -> (Int, Int) -> (Int, Int)
ugolCubieRotate np p = let
                      cycles np | np == downSide  = rotateOr (5, 0) (4, 0) (7, 0) (6, 0)
                                | np == frontSide = rotateOr (5, 2) (1, 1) (0, 2) (4, 1)
                                | np == leftSide  = rotateOr (2, 1) (1, 2) (5, 1) (6, 2)
                                | np == rightSide = rotateOr (4, 2) (0, 1) (3, 2) (7, 1)
                                | np == backSide  = rotateOr (7, 2) (3, 1) (2, 2) (6, 1)
                                | np == upSide    = rotateOr (0, 0) (1, 0) (2, 0) (3, 0)
                    in
                      case cycles np p of (n, orient) -> (n, orient `mod` 3)
                      
                      
-- =====================================================================
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

rebroCubieRotate :: Povorot -> (Int, Int) -> (Int, Int)
rebroCubieRotate np p = let
                          cycles np | np == downSide  = rotateOr (4, 0) (7, 0) (6, 0) (5, 0)
                                    | np == frontSide = rotateOr (4, 1) (9, 1) (0, 1) (8, 1)
                                    | np == leftSide  = rotateOr (1, 0) (9, 0) (5, 0) (10, 0)
                                    | np == rightSide = rotateOr (8, 0) (3, 0) (11, 0) (7, 0)
                                    | np == backSide  = rotateOr (11, 1) (2, 1) (10, 1) (6, 1)
                                    | np == upSide    = rotateOr (0, 0) (1, 0) (2, 0) (3, 0)
                         in
                           case cycles np p of (n, orient) -> (n, orient `mod` 2)
                           
-- ====================================================================================

data CubieU = CubieU {getNumberU :: Int, getOrientU :: Int} deriving Show
data CubieR = CubieR {getNumberR :: Int, getOrientR :: Int} deriving Show
data Kub = Kub {getUgol :: [CubieU], getRebro :: [CubieR]} deriving Show

defaultRebro :: [CubieR]
defaultRebro = map (`CubieR` 0)  [0..11]

defaultUgol :: [CubieU]
defaultUgol = map (`CubieU` 0)  [0..7]

mkFromUgolPerm :: [Int] -> Kub
mkFromUgolPerm u = Kub (map (`CubieU` 0) u) defaultRebro 

mkFromRebroPerm :: [Int] -> Kub
mkFromRebroPerm r = Kub defaultUgol $ map (`CubieR` 0) r

mkFromUgolOr :: [Int] -> Kub
mkFromUgolOr u = Kub (zipWith CubieU [0..] u) defaultRebro                 
                 
mkFromRebroOr :: [Int] -> Kub
mkFromRebroOr r = Kub defaultUgol $ zipWith CubieR [0..] r

projectUgolOr :: Kub -> [Int]
projectUgolOr = map getOrientU . getUgol

projectRebroOr :: Kub -> [Int]
projectRebroOr = map getOrientR . getRebro

projectUgolPerm :: Kub -> [Int]
projectUgolPerm = map getNumberU . getUgol

projectRebroPerm :: Kub -> [Int]
projectRebroPerm = map getNumberR . getRebro


permutations :: (Povorot -> (Int, Int) -> (Int, Int)) -> Povorot -> [Int] -> [Int]
permutations cubieMover np = map $ fst . cubieMover np . (, 0) 


orientations :: (Povorot -> (Int, Int) -> (Int, Int)) -> Povorot -> [Int] -> [Int]
orientations cubieMover np = map snd . sortOn fst . map (cubieMover np) . zip [0..]


ugolPerm :: Povorot -> [Int] -> [Int]
ugolPerm = permutations ugolCubieRotate

rebroPerm :: Povorot -> [Int] -> [Int]
rebroPerm = permutations rebroCubieRotate


ugolOr :: Povorot -> [Int] -> [Int]
ugolOr = orientations ugolCubieRotate


rebroOr :: Povorot -> [Int] -> [Int]
rebroOr = orientations rebroCubieRotate


mainRotatesToFullRotates :: (Povorot -> Kub -> Kub) -> Povorot -> Kub -> Kub
mainRotatesToFullRotates rotater np | np == 0               = id 
                                    | np > 18               = error $ " np > 18: np=" ++ show np
                                    | (np - 1) `mod` 3 == 2 = let t = mainRotatesToFullRotates rotater (np-2) in t . t
                                    | (np - 1) `mod` 3 == 1 = let t = mainRotatesToFullRotates rotater (np-1) in t . t . t 
                                    | (np - 1) `mod` 3 == 0 = rotater np


rotateKubMainAxis :: Povorot -> Kub -> Kub
rotateKubMainAxis np (Kub u r) = let
                        uPerm = ugolPerm np $ map getNumberU u
                        uOr = ugolOr np $ map getOrientU u
                        rPerm = rebroPerm np $ map getNumberR r
                        rOr = rebroOr np $ map getOrientR r
                      in 
                        Kub (zipWith CubieU uPerm uOr) (zipWith CubieR rPerm rOr)

rotate :: Povorot -> Kub -> Kub
rotate = mainRotatesToFullRotates rotateKubMainAxis


moveInCoordsSlow :: (Pos -> Kub) -> (Kub -> Pos) -> Povorot -> Pos -> Pos
moveInCoordsSlow from to np = to . rotate np . from 

x2MoveSlow :: Povorot -> Pos -> Pos
x2MoveSlow = moveInCoordsSlow (mkFromUgolPerm . fromX2) (toX2 . projectUgolPerm)

y2MoveSlow :: Povorot -> Pos -> Pos
y2MoveSlow = let 
                extend = (++ [8, 9, 10, 11])
                proj   = take 8
             in 
                moveInCoordsSlow (mkFromRebroPerm . extend . fromY2) (toY2 . proj . projectRebroPerm)


z2MoveSlow :: Povorot -> Pos -> Pos
z2MoveSlow = let 
                extend = ([0..7] ++) . map (+8)
                proj   = map (\x -> x - 8) . drop 8
             in 
                moveInCoordsSlow (mkFromRebroPerm . extend . fromZ2) (toZ2 . proj . projectRebroPerm)


x1MoveSlow :: Povorot -> Pos -> Pos
x1MoveSlow = moveInCoordsSlow (mkFromUgolOr . fromX1) (toX1 . projectUgolOr)


y1MoveSlow :: Povorot -> Pos -> Pos
y1MoveSlow = moveInCoordsSlow (mkFromRebroOr . fromY1) (toY1 . projectRebroOr)