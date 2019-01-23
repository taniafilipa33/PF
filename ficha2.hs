import Data.Char
import Data.List

--Ficha2

--1

--a

funA :: [Double] -> Double
funA [] = 0
funA (y:ys) = y^2 + (funA ys)
 ------39 em [2.3.5.1]

 --b)

funB :: [Int] -> [Int]
funB [] = []
funB (h:t) = if (mod h 2)==0 then h : (funB t) else (funB t)
------------funB [8,5,12] --8,12

--c)

funC (x:y:t) = funC t
funC [x] = []
funC [] = []
------- funC [1,2,3,4,5] --[]

--d)


g l [] = l
g l (h:t) = g (h:l) t
funD l = g [] l
------------ funD "otrec"-> certo

--2)

--a)

dobros :: [Float] -> [Float]
dobros [] = []
dobros (l:ls)= (2*l): dobros ls

--b)

numOcorre :: Char -> String -> Int
numOcorre a []= 0
numOcorre a (l:ls)|l==a = 1+numOcorre a ls
                  |otherwise = numOcorre a ls

--c)

positivos :: [Int] -> Bool
positivos [] = True
positivos (l:ls) | l<=0 = False
                 | otherwise = positivos ls

--d)

soPos :: [Int] -> [Int] 
soPos []= []
soPos (l:ls) | l<0 = soPos ls
             | otherwise = l:soPos ls

--e)

somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (l:ls)| l<0 = l+somaNeg ls
              |otherwise = somaNeg ls

--f)

tresUlt :: [a] -> [a]
tresUlt (l:ls)|length(l:ls)<=3 = (l:ls)
              |otherwise = tresUlt ls

--g)

segundos :: [(a,b)] -> [b]
segundos [] = []
segundos (l:ls)= snd l:segundos ls

--h)

nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros _ [] = False
nosPrimeiros a (l:ls) | a== fst l = True
                      |otherwise = nosPrimeiros a ls

--i)
auxSoma :: (Num a, Num b, Num c) => (a,b,c) -> (a,b,c) -> (a,b,c)
auxSoma (a,b,c) (x,y,z) = (a+x,b+y,c+z)

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos [(a,b,c)] = (a,b,c)
sumTriplos (l:ls)= sumTriplos ((auxSoma l (head ls)):(tail ls))


--3

--a)

soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (l:ls) | isDigit(l) == True = l : soDigitos ls
                 |otherwise = soDigitos ls

--b)

minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (l:ls) | isLower(l) == True = 1+minusculas ls
                  | otherwise = minusculas ls

--c)

nums :: String -> [Int]
nums [] = []
nums (l:ls) | isDigit(l) == True = digitToInt(l) : nums ls
            |otherwise = nums ls


--4)

type Polinomio = [Monomio]
type Monomio = (Float,Int)

--a 

conta :: Int -> Polinomio -> Int 
conta _ [] = 0
conta x (l:ls)| x == snd l = 1+conta x ls
              |otherwise = conta x ls

--b)

grau :: Polinomio -> Int
grau [] = 0
grau [x] = snd x
grau (l:ls) | snd l > snd (head ls) = grau (l:(tail ls))
            |otherwise = grau ls 

--c)

selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau n (l:ls) | n== snd l = l:(selgrau n ls)
                 |otherwise = selgrau n ls


--d)

deriv :: Polinomio -> Polinomio
deriv [] = []
deriv (l:ls) = (((fst l)*fromIntegral(snd l)),(snd l)-1): deriv ls 

--f)

simp :: Polinomio -> Polinomio
simp [] = []
simp (l:ls) | snd l ==0 = simp ls
            |otherwise = l:simp ls

--g)

mult :: Monomio -> Polinomio -> Polinomio
mult (0,0) _ = []
mult _ [] = []
mult (a,b) (l:ls) = (a*fst l , b+ snd l) : mult (a,b) ls

--h)

auxNormaliza :: Monomio -> Polinomio -> Polinomio
auxNormaliza a [] = []
auxNormaliza (a,b) (l:ls)| b== snd l =((a+fst l), b):ls
                         |otherwise = l:(auxNormaliza (a,b) ls)


normaliza :: Polinomio -> Polinomio 
normaliza [] = []
normaliza (l:ls)= normaliza (auxNormaliza l ls)


--i)

--soma :: Polinomio -> Polinomio -> Polinomio

--j)

produto :: Polinomio -> Polinomio -> Polinomio
produto [] _ = []
produto _ [] = []
produto (l:ls) x = produto ls (mult l x)