{-# LANGUAGE FlexibleContexts #-}
module Test where
import Control.Monad.Except
import SimpleTables
import CubieCoord
import CoordsMovesSlow
import KubTypes
import KubGeometry
import KubSolver
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
   return ()

unixDiffTimeToMicros :: UnixDiffTime -> Integer
unixDiffTimeToMicros diff = ((* 1000000) . Data.Ratio.numerator .toRational . udtSeconds $ diff)
                            + (toInteger . udtMicroSeconds $ diff)

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


chart :: UArray Pos Depth -> [Int]
chart table = takeWhile (/= 0) $ map (length . f) [0..] where
  list = assocs table
  f depth = filter (\(_, d) -> d == depth) list


prettyPrint :: Show a => [a] -> IO ()
prettyPrint = putStrLn . concatMap ((++ ['\n']) . show)         
                   
          
validPos = do
            x <- [0..x2Max]
            y <- [0..y2Max]
            z <- [0..z2Max]
            guard $ posValidator (x, y, z)
            return (x, y, z)



posValidator (x, y, z) = even $ perestParity (fromX2 x) + perestParity (fromY2 y) + perestParity (fromZ2 z)


rnd :: Int -> IO Int
rnd b = fmap (`mod` b) randomIO

rndPred :: (a -> Bool) -> IO a -> IO a
rndPred p rnd = rnd >>= \a -> if p a
                              then return a
                              else rndPred p rnd


rndPos2 = let
           r = do
               x <- rnd x2Max
               y <- rnd y2Max
               z <- rnd z2Max
               return (x, y, z)
         in
           rndPred posValidator r

rndPos1 = do
            x <- rnd x1Max
            y <- rnd y1Max
            z <- rnd z1Max
            return (x, y, z)


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

d = 1
d' = 2
d2 = 3
f = 4
f' = 5
f2 = 6
l = 7
l' = 8
l2 = 9
r = 10
r' = 11
r2 = 12
b = 13
b' = 14
b2 = 15
u = 16
u' = 17
u2 = 18

singleMoveInverse :: Int -> Int
singleMoveInverse m | m == 0 = 0 | (m - 1) `mod` 3 == 2 = m | (m - 1) `mod` 3 == 0 = m + 1 | (m - 1) `mod` 3 == 1 = m - 1
                    
inverseMoves :: [Int] -> [Int]
inverseMoves = map singleMoveInverse . reverse  
               
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
                  