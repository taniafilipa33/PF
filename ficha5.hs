--ficha 5

import Data.Char

--1

--a)

myany :: (a -> Bool) -> [a] -> Bool 
myany i [] = False
myany i (l:ls) = if i l == True then True else myany i ls 

--b)

myzipWith :: (a->b->c) -> [a] -> [b] -> [c] 
myzipWith i [] _ = []
myzipWith i _ [] = []
myzipWith i (l:ls) (x:xs)= ( i l x):(myzipWith i ls xs)

--c)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (h:ts) = if (f h) then h : (takeWhile' f ts) else []

--d)

mydropWhile :: (a->Bool) -> [a] -> [a]
mydropWhile i [] = []
mydropWhile f (h:ts) = if (f h) then mydropWhile f ts else (h:ts)

--e)

myspan :: (a-> Bool) -> [a] -> ([a],[a])
myspan i [] = ([],[])
myspan i (x:xs) | i x == True = (x:a,b)
                | i x == False = ([], (x:xs)) where 
                (a,b) = myspan i xs

--f)

deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a] 
deleteBy i _ [] = []
deleteBy i a (x:xs) | i a x == True = xs
                    |otherwise      = x:(deleteBy i a xs)

--g)

--mysortOn :: Ord b => (a -> b) -> [a] -> [a]


--2)

type Polinomio = [Monomio]
type Monomio = (Float,Int)

--a)

selgrau :: Int -> Polinomio  -> Polinomio
selgrau _  [] = []
selgrau x (l:ls) | x==snd l = l:(selgrau x ls)
                 | otherwise= selgrau x ls

--b)

conta :: Int -> Polinomio -> Int
conta _ [] = 0
conta a (l:ls) | a==snd l = 1 +( conta a ls)
               | otherwise = conta a ls

--c)

grau :: Polinomio -> Int
grau [] = 0
grau [x] = snd x
grau (x:xs) | snd x >  snd (head xs) = grau (x:(tail xs))
            |otherwise = grau xs       

--d)

deriv :: Polinomio -> Polinomio
deriv [] = []
deriv (x:xs) = (((fst x)*(fromIntegral(snd x))),(snd x)-1):(deriv xs)         

--e)

calcula :: Float -> Polinomio -> Float
calcula x [] = 0
calcula x (l:ls) = ((x*(fst l))^(snd l))+ (calcula x ls)

--f)

simp :: Polinomio -> Polinomio
simp [] = []
simp (l:ls) | fst l == 0 = simp ls
            |otherwise = l : (simp ls)

--g)

mult :: Monomio -> Polinomio -> Polinomio         
mult _ [] = []
mult (0,_) _ = []
mult x (l:ls) = ((fst x * fst l),(snd x + snd l)):(mult x ls)

--h)

ordena :: Polinomio -> Polinomio
ordena [] = []
ordena [x] = [x]
ordena l = (maisP l): (ordena (remove (maisP l) l)) where
      maisP ::  Polinomio -> Monomio
      maisP [] = (0,0)
      maisP [x] = x
      maisP (x:xs) | snd x <= snd (head xs) = maisP (x:(tail xs))
                   | otherwise = maisP xs
      remove :: Monomio -> Polinomio ->Polinomio
      remove x [] = []
      remove (a,b) ((x,y):ls) | a==x && b==y = ls
                              |otherwise =( x,y):(remove (a,b) ls)


--i)

normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza l = norm (ordena l) where
              norm :: Polinomio -> Polinomio
              norm [] = []
              norm [x] = [x]
              norm ((a,b):ls)| b==snd(head ls) = norm((a+(fst (head ls)),b):(tail ls))
                             | otherwise =(a,b):(norm ls)                        

--j)

soma :: Polinomio -> Polinomio -> Polinomio                        
soma [] x = x
soma x [] = x
soma x l = normaliza (x++l)


--k)

produto :: Polinomio -> Polinomio -> Polinomio
produto [] x = x
produto x [] = x
produto (x:xs) l =( mult x l) ++ (produto xs l)


--l)

equiv :: Polinomio -> Polinomio -> Bool
equiv l z = if normaliza l == normaliza z then True else False


--3

type Mat a = [[a]]

--a)

dimOk :: Mat a -> Bool
dimOk [] = True
dimOK (l:ls)| length l /= length (head ls)  || length l <0 = False
            | otherwise = dimOk ls

--b) 

dimMat :: Mat a -> (Int,Int)
dimMat [] = (0,0)
dimMat l | dimOk l == True = (length (head l), length l) 
         |otherwise = (0,0)        

--c)

addMat :: Num a => Mat a -> Mat a -> Mat a
addMat [] x = x
addMat x [] = x
addMat (l:ls) (t:ts) = (summ l t ) : (addMat ls ts) where
                       summ :: Num a => [a] -> [a] ->[a]
                       summ [] x = x
                       summ x [] = x
                       summ (l:ls) (x:xs) = (l+x):(summ ls xs)   

--d)

transpose :: Mat a -> Mat a
transpose [] = []
transpose mat = if (length (head mat) > 1) then (map head mat) : (transpose (map tail mat)) else [(map head mat)]

--e)

multMat :: Num a => Mat a -> Mat a -> Mat a  
multMat [] _ = []
multMat h (x:xs) = [sum (zipWith (*) (head h) (head (transpose(x:xs)) )) ,sum (zipWith (*) (head h) (head (tail (transpose(x:xs))) )) ]  :( multMat (tail h) (x:xs))


--f)

zipWMat :: (a -> b -> c) -> Mat a -> Mat b -> Mat c
zipWMat f mA mB = zipWith (zipWith f) mA mB

--g)


triSup :: (Num a ,Eq a)=> Mat a -> Bool
triSup [] = False
triSup mat = tAux 0 mat where
    tAux :: (Eq a ,Num a) => Int -> Mat a -> Bool
    tAux x [] = True
    tAux x (h:ts) = if ((length (fst (span (0 ==) h))) >= x) then tAux (x + 1) ts else False

--h)


rotateLeft :: Mat a -> Mat a    
rotateLeft [] = []
rotateLeft l = if length (head l )>1 then [(map (last) l)]++ (rotateLeft (map init l)) else [(map head l)]