module Cubie(CubieSet, getCubieSet) where
import MathComb


data CubieSetErrors = UgolOrientErr | RebroOrientErr | UgolPerestErr | RebroPerestErr| ParityError deriving Show
data CubieSet = CubieSet [Z] [Z] [Z] [Z] deriving (Show, Eq)

getCubieSet :: [Z] -> [Z] -> [Z] -> [Z] -> Either CubieSetErrors CubieSet
getCubieSet uo up ro rp = do
 let wrap e cubie checker = if checker cubie then Right () else Left e
 let wrapParity up rp = if checkParity up rp then Right () else Left ParityError
 wrap RebroPerestErr rp checkRebroPer
 wrap RebroOrientErr ro checkRebroOr
 wrap UgolPerestErr up checkUgolPer
 wrap UgolOrientErr uo checkUgolOr
 wrapParity up rp
 return $ CubieSet uo up ro rp
  


checkRebroOr :: [Z] -> Bool
checkRebroOr ls = if ( length ls == 12) && 
                     (all  (\x -> x<2 && x>=0) ls) && 
                     (foldr (+) 0  ls) `mod` 2 == 0 then True
                else False

checkUgolOr :: [Z] -> Bool
checkUgolOr ls = if (length ls == 8) && 
                  (all  (\x -> x<3 && x>=0) ls) &&
                  (foldr (+) 0   ls) `mod` 3 == 0 then True
               else False
 
checkUgolPer :: [Z] -> Bool
checkUgolPer ls = if length ls == 8 &&
                   present ls 0 then True
                else False where
 present _ 8 = True
 present ls n = (any (==n) ls) && (present ls (n+1))

checkRebroPer :: [Z] -> Bool
checkRebroPer ls = if length ls == 12 &&
                   present ls 0 then True
                else False where
 present _ 12 = True
 present ls n = (any (==n) ls) && (present ls (n+1))

checkParity :: [Z] -> [Z] -> Bool
checkParity u r = ((perestParity u + perestParity r) `mod` 2) == 0