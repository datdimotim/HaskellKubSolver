module MoveTableBuilder (buildMoveTable, moverFast, mover2Fast, buildMover, buildMover2)  where 

import KubTypes

import Data.Array.Unboxed
import qualified Data.Array.Unboxed as Array


-------------- HELPERS -----------------------------
moverFast  :: UArray (Povorot, Pos) Pos -> Pos -> [Pos]
moverFast  arr pos = map (\pov -> moveInCoords arr pov pos) [1..18]

mover2Fast :: UArray (Povorot, Pos) Pos -> Pos -> [Pos]
mover2Fast arr pos = map (\pov -> moveInCoords arr pov pos) [1..10]

buildMover :: (Povorot -> Pos -> Pos) -> (Pos -> [Pos])
buildMover single pos = map (`single` pos) [1 .. 18]

buildMover2 :: (Povorot -> Pos -> Pos) -> (Pos -> [Pos])
buildMover2 single pos = map (`single` pos) [1, 2, 3, 6, 9, 12, 15, 16, 17, 18]
----------------------------------------------------

 
buildMoveTable :: MaxPos -> (Pos -> [Pos]) -> UArray (Povorot, Pos) Pos
buildMoveTable maxPos mover = array ((1, 0), (movesCount, maxPos)) table where
 movesCount = length $ mover 0 
 table = do
         pos <- [0..maxPos]
         (np, p')  <- zip [1..] (mover pos)
         return ((np, pos), p')


-------------- PRIVATE HELPERS ----------------------

moveInCoords :: UArray (Povorot, Pos) Pos -> Povorot -> Pos -> Pos
moveInCoords arr pov pos = arr Array.! (pov,pos)
