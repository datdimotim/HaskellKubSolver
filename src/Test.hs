{-# LANGUAGE FlexibleContexts #-}
module Test where
import Control.Monad.Except
import SimpleTables
import CubieCoord
import CoordsMovesSlow
import KubTypes
import KubGeometry
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
   --Right except <- runExceptT $ testAll `catchError` return
   --putStrLn except
   --print $ length (elems moveTable) 
   --print $ depths
   print $ chart x2DeepTable
   print $ chart y2DeepTable
   print $ chart z2DeepTable
   print $ chart x1DeepTable
   print $ chart y1DeepTable
   print $ chart z1DeepTable
   print "=============="
   prettyPrint $ take 20 $ map (\p -> (p, head $ solver2Phase p)) validPos
   p <- rndPos2
   print p
   print (head $ solver2Phase p)
   
   p1 <-rndPos1
   print p1
   print (head $ solver1Phase p1)
   finishTime <- getUnixTime
   kub <- rndKub
   print $ projectUgolPerm kub
   print $ projectRebroPerm kub
   print $ projectUgolOr kub
   print $ projectRebroOr kub
   print (head $ kubSolver kub)
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
applyMoves [] k = k
applyMoves (m:ms) k = applyMoves ms (rotate m k)

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
                

prettyPrint :: Show a => [a] -> IO ()
prettyPrint = putStrLn . concatMap ((++ ['\n']) . show)

nextVars2Phase :: N -> [N]
nextVars2Phase (N (x, y, z) is d) = let
                                  pred = if null is then 0 else head is
                                  moveWithId p x y z = if p == 0
                                                       then (x, y, z)
                                                       else (x2MoveTable ! (p, x), y2MoveTable ! (p, y), z2MoveTable ! (p, z))
                              in 
                                do 
                                  p <- [0..10]
                                  guard $ hodPredHod pred p 
                                  let (x', y', z') = moveWithId p x y z
                                  let xd = x2DeepTable ! x'
                                  let yd = y2DeepTable ! y'
                                  let zd = z2DeepTable ! z'
                                  let dEst = maximum [xd, yd, zd] 
                                  guard (dEst <= d-1)
                                  return $ N (x', y', z') (p:is) (d-1)
                                  
nextVars1Phase :: N -> [N]
nextVars1Phase (N (x, y, z) is d) = let
                                  pred = if null is then 0 else head is
                                  moveWithId p x y z = if p == 0
                                                       then (x, y, z)
                                                       else (x1MoveTable ! (p, x), y1MoveTable ! (p, y), z1MoveTable ! (p, z))
                              in 
                                do 
                                  p <- [0..18]
                                  guard $ hodPredHod pred p 
                                  let (x', y', z') = moveWithId p x y z
                                  let xd = x1DeepTable ! x'
                                  let yd = y1DeepTable ! y'
                                  let zd = z1DeepTable ! z'
                                  let dEst = maximum [xd, yd, zd] 
                                  guard (dEst <= d-1)
                                  return $ N (x', y', z') (p:is) (d-1)


validPos = do
            x <- [0..x2Max]
            y <- [0..y2Max]
            z <- [0..z2Max]
            guard $ posValidator (x, y, z)
            return (x, y, z)

m10to18 :: Int -> Int
m10to18 m | m == 0 = 0 
          | otherwise = [1,2,3,6,9,12,15,16,17,18] !! (m-1)

posValidator (x, y, z) = even $ perestParity (fromX2 x) + perestParity (fromY2 y) + perestParity (fromZ2 z)

hodPredHod 0 _ = True
hodPredHod p h = let
                   r = (`div` 3) . (\x -> x-1) 
                   b1 = p /= 0 && h == 0
                   b2 = r p == r h 
                   b3 = r p == 0 && r h == 5
                   b4 = r p == 1 && r h == 4
                   b5 = r p == 2 && r h == 3
                 in
                   not $ or [b1, b2, b3, b4, b5]

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


