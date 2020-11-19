{-# LANGUAGE FlexibleContexts #-}
module Test where
import Control.Monad.Except
import SimpleTables
import CubieCoord
import CoordsMovesSlow
import KubTypes
import KubGeometry
import KubSolver
import KubUtils
import Data.Array.Unboxed
import Data.Array.MArray
import Data.Array.ST
import Control.Monad.ST
import ListT hiding (take, head, null)
import Data.Foldable ()
import System.Random
import System.IO
import Data.UnixTime
import Data.Ratio
import MathComb
import Benchmark



testEntryPoint :: IO ()
testEntryPoint = do
   hSetBuffering stdout NoBuffering
   startTime <- getUnixTime
   putStrLn "Starting"
   print $ chart x2DeepTable
   print $ chart y2DeepTable
   print $ chart z2DeepTable
   print $ chart x1DeepTable
   print $ chart y1DeepTable
   print $ chart z1DeepTable
   print "=============="
   kub <- rndKub
   putStrLn $ "ugolPerm: " ++ show (projectUgolPerm kub)
   putStrLn $ "ugolOr: " ++  show (projectRebroPerm kub)
   putStrLn $ "rebroPerm: " ++ show (projectUgolOr kub)
   putStrLn $ "rebroOr: " ++ show (projectRebroOr kub)
   let solution = head $ kubSolver kub
   print solution
   putStrLn $ "solution is ok: " ++ show (solvedKub == applyMoves solution kub)
   putStrLn $ "manual full test: " ++ show testKubSolver
   finishTime <- getUnixTime
   let diffMicros = unixDiffTimeToMicros $ finishTime `diffUnixTime` startTime
   putStrLn $ "Total time: " ++ show (diffMicros `div` 1000) ++ "ms"
   putStrLn ""
   putStrLn "benchmarking"
   --withTimer 100000
   testSetBenchmark
   return ()


testAll :: ExceptT String IO String
testAll = do
    testIO "testX1" x1Max (toX1.fromX1)
    testIO "testY1" y1Max (toY1.fromY1)
    testIO "testZ1" z1Max (toZ1.fromZ1)
    testIO "testX2" x2Max (toX2.fromX2)
    testIO "testY2" y2Max (toY2.fromY2)
    testIO "testZ2" z2Max (toZ2.fromZ2)
    return "All tests passed"



testIO name bnd f =  do
                       r <- test name bnd f
                       liftIO $ print r
                       return ()




test :: MonadError String m => String -> MaxPos -> (Pos -> Pos) -> m String
test name bnd f = if all (\ i -> i == f i) [0 .. bnd] then
                      return (name ++ " OK...")
                  else
                      throwError (name ++ " failed!!!")
                     

solvedKub = Kub (zipWith CubieU [0..7] (replicate 8 0)) (zipWith CubieR [0..11] (replicate 12 0))

testRotate = let
               k1 = projectToCoords1 solvedKub
               k2 = projectToCoords2 solvedKub
               kub' = rotate 16 . rotate 10 $ solvedKub
             in
               k1 == (0, 0, 0) 
               && k2 == (0, 0, 0) 
               && [1,2,0,0,1,0,0,2] == projectUgolOr kub'
               && [0,4,1,2,7,5,6,3] == projectUgolPerm kub'
 
               
scrambledKub= let
               moves =  [u2, l', b2, d', l', b', f2, u', r, f, f2, r2, f2, u, b2, u', f2, d, b2, r2, d]
               scrambed  = applyMoves (inverseMoves moves) solvedKub
             in
               scrambed
               
testKubSolver = [2,0,5,7,1,4,6,3] == projectUgolPerm scrambledKub
                && [2,1,1,0,1,1,0,0] == projectUgolOr scrambledKub
                && [0,5,8,1,2,9,6,11,3,4,10,7] == projectRebroPerm scrambledKub
                && [1,0,1,1,0,1,1,0,1,1,1,0] == projectRebroOr scrambledKub
                && solvedKub == ((`applyMoves` scrambledKub) . head . kubSolver $ scrambledKub)
                  