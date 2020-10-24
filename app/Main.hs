{-# LANGUAGE FlexibleContexts #-}
module Main where
import Control.Monad.Except
import SimpleTables
import CubieCoord
import KubGeometry
import KubTypes
import Data.Array.Unboxed
import Data.Array.MArray
import Data.Array.ST
import Control.Monad.ST
import ListT hiding (take, head, null)
import Data.Foldable ()
import qualified Data.Map.Strict as Map
import System.Random
import System.IO

import MathComb



main :: IO ()
main = do
   hSetBuffering stdout NoBuffering
   --Right except <- runExceptT $ testAll `catchError` return
   --putStrLn except
   --print $ length (elems moveTable) 
   --print $ depths
   print $ chart x2DeepTable
   print $ chart y2DeepTable
   print $ chart z2DeepTable
   print "=============="
   prettyPrint $ take 100 $ map solverPos validPos
   p <- rndPos
   print p
   print $ solver1 p
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






chart :: UArray Pos Depth -> [Int]
chart table = takeWhile (/= 0) $ map (length . f) [0..] where
  list = assocs table
  f depth = filter (\(_, d) -> d == depth) list




backtrack :: (a -> [a]) -> (a -> Bool) -> a -> [a]
backtrack c p i = filter p [i] ++ concatMap (backtrack c p) (c i)

type Idx = Int

data N = N { 
             position :: (Int, Int, Int),
             moves  :: [Idx],
             depth :: Depth
           } deriving (Show)



isSolved :: N -> Bool
isSolved (N (x, y, z) _ _) = x2DeepTable ! x + y2DeepTable ! y + z2DeepTable ! z == 0


solver p = let 
          solutions = backtrack nextVars isSolved (N p [] 30)
          nullFilter = takeWhile (/=0)
        in  
          map (\(N p m d) -> N p (nullFilter m) d) solutions
                

solver1 = head . solver

solverPos p = (p, solver1 p)

prettyPrint :: Show a => [a] -> IO ()
prettyPrint = putStrLn . concatMap ((++ ['\n']) . show)

nextVars :: N -> [N]
nextVars (N (x, y, z) is d) = let
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


validPos = do
            x <- [0..x2Max]
            y <- [0..y2Max]
            z <- [0..z2Max]
            guard $ posValidator (x, y, z)
            return (x, y, z)

m10to18 m = [1,2,3,6,9,12,15,16,17,18] !! (m-1)

posValidator (x, y, z) = even $ perestParity (fromX2 x) + perestParity (fromY2 y) + perestParity (fromZ2 z)

hodPredHod 0 h = True
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


rndPos = let
           r = do
               x <- rnd x2Max
               y <- rnd y2Max
               z <- rnd z2Max
               return (x, y, z)
         in
           rndPred posValidator r
