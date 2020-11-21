{-# LANGUAGE BangPatterns #-}

module KubSolver(kubSolver, applyMoves) where

import KubGeometry
import KubTypes
import CoordsMovesSlow
import SimpleTables
import Control.Monad (guard)
import Data.Array.Unboxed ((!))



backtrack :: (a -> [a]) -> (a -> Bool) -> a -> [a]
backtrack c p i = filter p [i] ++ concatMap (backtrack c p) (c i)

data N = N {
             position :: (Int, Int, Int),
             moves  :: [Int],
             getDepth :: Depth
           } deriving (Show)



isSolved :: N -> Bool
isSolved  = (==0) . getDepth

applyMoves :: [Int] -> Kub -> Kub
applyMoves ms k = foldl (flip rotate) k ms


kubSolver :: Kub -> [[Int]]
kubSolver kub =  let
                   k1 = projectToCoords1 kub
                   completeSolution ms = let
                                           kub' = applyMoves ms kub
                                           k2 = projectToCoords2 kub'
                                           ms2 = moves . head $ solver2Phase k2
                                         in
                                           ms ++ map m10to18 ms2
                 in
                   map (completeSolution . moves) (solver1Phase k1)


solver2Phase :: (Int, Int, Int) -> [N]
solver2Phase = mkSolver nextVars2Phase

solver1Phase :: (Int, Int, Int) -> [N]
solver1Phase = mkSolver nextVars1Phase

mkSolver :: (N -> [N]) -> (Int, Int, Int) -> [N]
mkSolver nextVars p = let
             solutions = backtrack nextVars isSolved (N p [] 30)
             nullFilter = takeWhile (/=0)
           in
             map (\(N p m d) -> N p (reverse . nullFilter $ m) d) solutions

nextVars2Phase :: N -> [N]
nextVars2Phase (N (!x, !y, !z) is !d) = let
                                  !pred = if null is then 0 else head is
                                  moveWithId !p !x !y !z = if p == 0
                                                       then (x, y, z)
                                                       else (x2MoveTable ! (p, x), y2MoveTable ! (p, y), z2MoveTable ! (p, z))
                              in
                                do
                                  p <- [0..10]
                                  guard $ hodPredHod (m10to18 pred) (m10to18 p)
                                  let (!x', !y', !z') = moveWithId p x y z
                                  let !xd = x2DeepTable ! x'
                                  let !yd = y2DeepTable ! y'
                                  let !zd = z2DeepTable ! z'
                                  let !dEst = maximum [xd, yd, zd]
                                  guard (dEst <= d-1)
                                  return $ N (x', y', z') (p:is) (d-1)

nextVars1Phase :: N -> [N]
nextVars1Phase (N (!x, !y, !z) is !d) = let
                                  !pred = if null is then 0 else head is
                                  moveWithId !p !x !y !z = if p == 0
                                                       then (x, y, z)
                                                       else (x1MoveTable ! (p, x), y1MoveTable ! (p, y), z1MoveTable ! (p, z))
                              in
                                do
                                  p <- [0..18]
                                  guard $ hodPredHod pred p
                                  let (!x', !y', !z') = moveWithId p x y z
                                  let !xd = x1DeepTable ! x'
                                  let !yd = y1DeepTable ! y'
                                  let !zd = z1DeepTable ! z'
                                  let !dEst = maximum [xd, yd, zd]
                                  guard (dEst <= d-1)
                                  return $ N (x', y', z') (p:is) (d-1)

m10to18 :: Int -> Int
m10to18 = ([0,1,2,3,6,9,12,15,16,17,18] !!)
         
hodPredHod :: Int -> Int -> Bool   
hodPredHod 0 _ = True
hodPredHod _ 0 = False
hodPredHod !p !h = let 
                   pa = (p - 1) `div` 3
                   ha = (h - 1) `div` 3
                   
                   r1 = pa == ha
                   r2 = pa == 5 && ha == 0
                   r3 = pa == 4 && ha == 1
                   r4 = pa == 3 && ha == 2 
                 in
                   not $ or [r1, r2, r3, r4]