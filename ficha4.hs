import Data.Char

--Ficha4

--2

--a  [x|x<-[1..1024],y<-[0..10], 2^y==x]
 
--b  [(x,y) | x<-[1..10],y<-[1..10], x+y==6] 

--c  

--3)
digitAlpha :: String -> (String,String)
digitAlpha [] = ([],[])
digitAlpha (h:ts) | isAlpha h = (h:a,b)
                  | isDigit h = (a,h:b)
                  | otherwise = (a,b) where (a,b) = digitAlpha ts


--4)

nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (l:ls) | l<0 = (1+n,y,z)
           | l==0 = (n,1+y,z)
           | l>0 = (n,y,1+z) where (n,y,z) = nzp ls

--5)
aux :: (Num a ,Eq a , Ord a)=> a-> a -> a
aux a b   | a==0 = 0
          | b==0 = 0
          | (a>=b)= (1+ (aux (a-b) b)) 
          | otherwise = 0


adivMod :: Integral a => a -> a -> (a, a)
adivMod 0 _ = (0,0)
adivMod _ 0 = undefined
adivMod a b = ((aux a b) ,a-(b*(aux a b)))

--6)
lengthy:: [Int] -> Int 
lengthy []=0
lengthy (l:ls) = 1 + lengthy ls

fromDigits :: [Int] -> Int
fromDigits [] = 0
fromDigits (h:t) = h*10^(lengthy t) + fromDigits t

--7)

maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit l = maximum [sum m | m <- inits l]