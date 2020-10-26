{-# LANGUAGE TupleSections #-}

module CoordsMovesSlow (x2MoveSlow, y2MoveSlow, z2MoveSlow, x1MoveSlow, y1MoveSlow) where

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