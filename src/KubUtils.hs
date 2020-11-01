module KubUtils where

import KubTypes
import Data.UnixTime
import Data.Ratio
import Data.Array.Unboxed
import KubGeometry
import MathComb
import CoordsMovesSlow
import CubieCoord
import System.Random (randomIO)


singleMoveInverse :: Int -> Int
singleMoveInverse m | m == 0 = 0 | (m - 1) `mod` 3 == 2 = m | (m - 1) `mod` 3 == 0 = m + 1 | (m - 1) `mod` 3 == 1 = m - 1
                    
inverseMoves :: [Int] -> [Int]
inverseMoves = map singleMoveInverse . reverse 


unixDiffTimeToMicros :: UnixDiffTime -> Integer
unixDiffTimeToMicros diff = ((* 1000000) . Data.Ratio.numerator .toRational . udtSeconds $ diff)
                            + (toInteger . udtMicroSeconds $ diff)
                            
                            
                            
chart :: UArray Pos Depth -> [Int]
chart table = takeWhile (/= 0) $ map (length . f) [0..] where
  list = assocs table
  f depth = filter (\(_, d) -> d == depth) list



rnd :: Int -> IO Int
rnd b = fmap (`mod` b) randomIO

rndPred :: (a -> Bool) -> IO a -> IO a
rndPred p rnd = rnd >>= \a -> if p a
                              then return a
                              else rndPred p rnd



rndKub :: IO Kub
rndKub = let
            validator k = even $ perestParity (projectRebroPerm k) + perestParity (projectUgolPerm k)
            r = do
              ugolPerest <- rnd x2Max
              rebroPerest <- rnd ((y2Max+1) * 9 * 10 * 11 * 12 - 1)
              ugolOr <- rnd x1Max
              rebroOr <- rnd y1Max
              return $ Kub (zipWith CubieU (fromX2 ugolPerest) (fromX1 ugolOr)) (zipWith CubieR ((`numberToFacNumber` 12) rebroPerest) (fromY1 rebroOr))
         in
           rndPred validator r