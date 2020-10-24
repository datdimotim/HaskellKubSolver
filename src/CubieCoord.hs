module CubieCoord ( 
                   toX1, fromX1,
                   toX2, fromX2,
                   toY1, fromY1,
                   toY2, fromY2,
                   toZ1, fromZ1,
                   toZ2, fromZ2) where
import MathComb

 
toX1 :: [Z] -> Z
toX1 (_:arr) = posNumberToNumber 3 arr 

fromX1 :: Z -> [Z]
fromX1 x = let tl = numberToPosNumber x 3 7
               s = sum tl
               h = (3 - (s `mod` 3)) `mod` 3
           in (h:tl)

toY1 :: [Z] -> Z
toY1 (_:arr) = posNumberToNumber 2 arr

fromY1 :: Z -> [Z]
fromY1 y = let tl = numberToPosNumber y 2 11
               s = sum tl
               h = (2 - (s `mod` 2)) `mod` 2
           in (h:tl)

toX2 :: [Z] -> Z
toX2 = facNumberToNumber

fromX2 :: Z -> [Z]
fromX2 x = numberToFacNumber x 8

toY2 :: [Z] -> Z
toY2 = facNumberToNumber

fromY2 :: Z -> [Z]
fromY2 y = numberToFacNumber y 8

toZ1 :: [Z] -> Z
toZ1 = cNumberToNumber

fromZ1 :: Z -> [Z]
fromZ1 x = numberToCNumber x 4 12

toZ2 :: [Z] -> Z
toZ2 = facNumberToNumber

fromZ2 :: Z -> [Z]
fromZ2 x = numberToFacNumber x 4


