{-# LANGUAGE TupleSections #-}

module DeepTableBuilder where

import KubTypes

import ListT hiding (null, repeat, take)
import Data.Array.Unboxed
import Data.Array.MArray
import Data.Array.ST
import Control.Monad.ST
import Control.Monad.State.Strict
import Data.Map.Strict hiding (map,filter,foldl,foldr,fold',null,take)
import qualified Data.Map.Strict as Map


-------------------- NAIVE VERSION ---------------------------------

buildDeepTable :: (Pos -> [Pos]) -> Map Pos Depth
buildDeepTable mover = fst $ execState (fixed [0]) (fromList [(0,0)], 0)  where
 fixed :: [Pos] -> State (Map Pos Depth, Depth) [Pos]
 fixed ps = if null ps 
             then return []
             else step >=> fixed $ ps
       

 step :: [Pos] -> State (Map Pos Depth, Depth) [Pos]
 step ps = do 
  (pairs, depth) <- get
  
  let newPs = do {
   p <- ps ;
   p <- mover p ;
   guard $ notMember p pairs ;
   return p ; }
  let pairs' = fromList (map (, depth + 1) newPs) `union` pairs
  let depth' = depth + 1  
  put (pairs', depth')
  return newPs 

-----------------------------------------------------------------------

------------------- ST Array version ----------------------------------
buildDeepTableST :: (Pos -> [Pos]) -> Array Pos Depth 
buildDeepTableST mover = runSTArray $ do
  arr <- fillSTArray (0,40319) 20
  writeArray arr 0 0
  ListT.toList . fixed arr $ 0 
  return arr
  where

  fixed :: STArray s Pos Depth -> Pos -> ListT (ST s) Pos
  fixed arr = let
                steps = map (step mover arr) [0..] 
              in                             
                foldr (>=>) return steps      
------------------------------------------------------------------------


step :: (Pos -> [Pos]) -> STArray s Pos Depth -> Depth -> Pos ->  ListT (ST s) Pos
step mover arr d p = do 
    p <- fromFoldable . mover $ p
    isUpdated <- lift $ testAndUpdate arr (d+1) p
    guard isUpdated
    return p 


---------------- HELPERS FOR access to ST Array ------------------------
fillSTArray :: (Ix i, MArray a e m) => (i, i) -> e -> m (a i e)
fillSTArray range e = let sz = rangeSize range
                          es = replicate sz e
                      in newListArray range es 



testAndUpdate :: STArray s Pos Depth -> Depth -> Pos -> ST s Bool
testAndUpdate arr d p = do
  dOld <- readArray arr p
  let needUpdate = dOld > d
  when needUpdate $ writeArray arr p d
  return needUpdate 
-------------------------------------------------------------------------





