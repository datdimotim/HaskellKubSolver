module MathComb where

type Z = Int
type Size = Z
type Base = Z 

posNumberToNumber :: Base -> [Z] -> Z
posNumberToNumber n = foldr (\x acc -> acc * n + x)  0

numberToPosNumber :: Z -> Base -> Size -> [Z]
numberToPosNumber 0 _ 0 = []
numberToPosNumber ch n size = (ch `mod` n) : numberToPosNumber (ch `div` n) n (size-1)
 
facNumberToNumber :: [Z] -> Z
facNumberToNumber [0]    = 0
facNumberToNumber (x:xs) = x * fac (length xs) + facNumberToNumber (norm x xs) where
 norm h = map $ \x -> if x < h then x else x-1


                       
numberToFacNumber :: Z -> Size -> [Z]
numberToFacNumber 0 1 = [0]
numberToFacNumber _ 1 = error "overflow"
numberToFacNumber x s = let 
                          w = fac (s-1)
                          r = x `mod` w
                          h = x `div` w
                          norm = map $ \x -> if x < h then x else x+1
                        in 
                          h : norm (numberToFacNumber r (s - 1))
 

fac 0 = 1
fac x = x * fac (x-1)

fac' m n | m>n = 1
         | m==n = n
         | otherwise = n * fac' m (n-1)

cmn :: Z -> Z -> Z
cmn m n  | m<=n && m>=0 = fac' (n-m+1) n `div` fac m
         | otherwise = 0

cNumberToNumber :: [Z] -> Z
cNumberToNumber ls = formula sel m where
 sel = reverse $ listSelected ls 0
 m = length sel
 listSelected []  _ = []
 listSelected (l:ls) i = if l==1 then i:listSelected ls (i+1) else listSelected ls (i+1)
 formula [] _  = 0
 formula (s:sel) i = cmn i (s+1) - cmn (i-1) s + formula sel (i-1)
 
numberToCNumber :: Z -> Size -> Size -> [Z]
numberToCNumber x  kol len = reverse $ helper (x+1) len kol where
 helper _ 0 _ = []
 helper x len kol | x > cmn kol (len-1) = 1:helper (x - cmn kol len + cmn (kol-1) (len-1)) (len-1) (kol-1)
                  | otherwise = 0:helper x (len-1) kol

perestParity :: [Z] -> Z
perestParity ps = helper ps 0 `mod` 2 where
 helper [_] _ = 0
 helper (p:ps) i | p == i = helper ps (i+1)
                 | otherwise = 1 + helper (map (\x -> if x==i then p else x) ps) (i+1)

