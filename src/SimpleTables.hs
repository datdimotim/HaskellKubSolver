module SimpleTables where 

import KubTypes
import MoveTableBuilder
import DeepTableBuilder
import KubGeometry

import Data.Array.Unboxed


----------------------------------------------------------------
x2MoveTable :: UArray (Povorot, Pos) Pos
x2MoveTable  = buildMoveTable x2Max $ buildMover2 x2MoveSlow


x2DeepTable :: UArray Pos Depth
x2DeepTable = toUnboxedArray $ buildDeepTableST (mover2Fast x2MoveTable)
----------------------------------------------------------------

----------------------------------------------------------------
y2MoveTable :: UArray (Povorot, Pos) Pos
y2MoveTable  = buildMoveTable y2Max $ buildMover2 y2MoveSlow


y2DeepTable :: UArray Pos Depth
y2DeepTable = toUnboxedArray $ buildDeepTableST (mover2Fast y2MoveTable)
----------------------------------------------------------------

----------------------------------------------------------------
z2MoveTable :: UArray (Povorot, Pos) Pos
z2MoveTable  = buildMoveTable z2Max $ buildMover2 z2MoveSlow


z2DeepTable :: UArray Pos Depth
z2DeepTable = toUnboxedArray $ buildDeepTableST (mover2Fast z2MoveTable)
----------------------------------------------------------------


--

toUnboxedArray arr = let
                       bnds = bounds arr
                       elms = elems arr
                     in 
                       listArray bnds elms

