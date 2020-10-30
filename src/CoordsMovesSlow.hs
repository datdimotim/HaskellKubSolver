{-# LANGUAGE TupleSections #-}

module CoordsMovesSlow (
                        x2MoveSlow, 
                        y2MoveSlow, 
                        z2MoveSlow, 
                        x1MoveSlow, 
                        y1MoveSlow, 
                        z1MoveSlow, 
                        projectToCoords1, 
                        projectToCoords2,
                        projectUgolPerm,
                        projectRebroPerm,
                        projectUgolOr,
                        projectRebroOr
                        ) where

import KubTypes
import CubieCoord
import KubGeometry

import Prelude hiding (foldr, foldl)
import Data.List hiding (permutations)


moveInCoordsSlow :: (Pos -> Kub) -> (Kub -> Pos) -> Povorot -> Pos -> Pos
moveInCoordsSlow from to np = to . rotate np . from 


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

projectToX2 :: Kub -> Int
projectToX2 = toX2 . projectUgolPerm

x2MoveSlow :: Povorot -> Pos -> Pos
x2MoveSlow = moveInCoordsSlow (mkFromUgolPerm . fromX2) projectToX2

projectToY2 :: Kub -> Int
projectToY2 = toY2 . take 8 . projectRebroPerm

y2MoveSlow :: Povorot -> Pos -> Pos
y2MoveSlow = let 
                extend = (++ [8, 9, 10, 11])
             in 
                moveInCoordsSlow (mkFromRebroPerm . extend . fromY2) projectToY2

projectToZ2 :: Kub -> Int
projectToZ2 = let 
                proj   = map (\x -> x - 8) . drop 8
              in
                toZ2 . proj . projectRebroPerm
                

z2MoveSlow :: Povorot -> Pos -> Pos
z2MoveSlow = let 
                extend = ([0..7] ++) . map (+8)
             in 
                moveInCoordsSlow (mkFromRebroPerm . extend . fromZ2) projectToZ2
                
projectToCoords2 :: Kub -> (Int, Int, Int)
projectToCoords2 kub = (projectToX2 kub, projectToY2 kub, projectToZ2 kub) 

projectToX1 :: Kub -> Int
projectToX1 = toX1 . projectUgolOr

x1MoveSlow :: Povorot -> Pos -> Pos
x1MoveSlow = moveInCoordsSlow (mkFromUgolOr . fromX1) projectToX1

projectToY1 :: Kub -> Int
projectToY1 = toY1 . projectRebroOr

y1MoveSlow :: Povorot -> Pos -> Pos
y1MoveSlow = moveInCoordsSlow (mkFromRebroOr . fromY1) projectToY1

projectToZ1 :: Kub -> Int
projectToZ1 = let
                rebroToZCubie = map $ \n -> if n >= 8 then 1 else 0 
              in 
                toZ1 . rebroToZCubie . reverse . projectRebroPerm

z1MoveSlow :: Povorot -> Pos -> Pos
z1MoveSlow = let
               rebroFromZCubie _ _ [] = []
               rebroFromZCubie ud mid (n:ns) = if n == 0
                                                 then ud : rebroFromZCubie (ud+1) mid ns 
                                                 else mid : rebroFromZCubie ud (mid+1) ns
             in 
               moveInCoordsSlow (mkFromRebroPerm . reverse . rebroFromZCubie 0 8 . fromZ1) projectToZ1
 
projectToCoords1 :: Kub -> (Int, Int, Int)
projectToCoords1 kub = (projectToX1 kub, projectToY1 kub, projectToZ1 kub)