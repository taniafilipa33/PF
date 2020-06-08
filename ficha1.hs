--Ficha 1
import Data.Char
--1

--a)

perimetro:: Float -> Float 
perimetro r = 2*pi*r

--b)


dist :: (Double,Double) -> (Double,Double)-> Double
dist (a,b) (x,y) = sqrt(((x - a)^2) + ((y - b)^2))


--c)

primUlt:: [a] -> (a,a)
primUlt l = (head l, last l)

--d)

multiplo :: Int -> Int -> Bool
multiplo m n =if mod m n == 0 then True else False

--e)

truncaImpar :: [a] -> [a]
truncaImpar [] = []
truncaImpar l = if mod (length l) 2 == 0 then l  else tail l

--f)

max2 :: Int -> Int -> Int
max2 a b = if a> b then a else b

--g)

max3 :: Int -> Int -> Int -> Int
max3 a b c = max2 (max2 a b) c


--2)

--a)

nRaizes :: (Floating a, Ord a) => a -> a -> a -> Int
nRaizes a b c | delta > 0  = 2
              | delta == 0 = 1
              | delta < 0 = 0
               where delta = (b**2) - (4 * a * c)

--b)

raizes :: (Floating a, Ord a) => a -> a -> a -> [a]
raizes a b c | nR == 0 = []
             | nR == 1 = [(-b) / (2 * a)]
             | nR == 2 = [((-b) + sqrt((b**2) - (4 * a * c))) / (2 * a), ((-b) - sqrt((b**2) - (4 * a * c))) / (2 * a)]
             where nR = nRaizes a b c


--3

type Hora = (Int, Int)

--a)
horaValida ::Hora-> Bool
horaValida (a,b) | 0 <= a && a < 24 && 0<=b && b<60 = True
                 |otherwise = False

--b)
horaCompara :: Hora -> Hora -> Bool
horaCompara (a,b) (x,y) | a>x = True
                        | a==x && b> y =True
                        | otherwise = False

--c)

horaToMin :: Hora -> Int
horaToMin (a,b) = b + (a*60)

--d)

minToHora :: Int -> Hora
minToHora a = (div a 60, mod a 60 )

--e) 

difHoras:: Hora -> Hora -> Int
difHoras (a,b) (x,y) | hC== True = horaToMin (a,b)- horaToMin(x,y)
                     |otherwise =  horaToMin (x,y)- horaToMin(a,b)
                     where hC = horaCompara (a,b) (x,y)

--f)

addMin :: Hora -> Int -> Hora
addMin (x,y) a = minToHora (a + horaToMin (x,y))

--4)

data Hora2 = H Int Int deriving (Show,Eq)

--5)

data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)

--a

next :: Semaforo -> Semaforo
next s| s== Verde = Amarelo
      |s== Amarelo = Vermelho
      |s==Vermelho = Verde

--b

stop :: Semaforo -> Bool
stop s | s==Vermelho = True
       |otherwise = False

--c)

safe :: Semaforo -> Semaforo -> Bool
safe a b | a==Vermelho || b==Vermelho = True
         |otherwise = False

--6

data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)

--a

posx :: Ponto -> Double
posx (Polar x y )= x * cos y
posx (Cartesiano x y )= x

--b)

posy :: Ponto -> Double
posy (Polar x y )= x * sin y
posy (Cartesiano x y )= y

--c

raio :: Ponto -> Double
raio (Polar x y )= x
raio (Cartesiano x y )= sqrt(x^2+y^2)

--d

angulo :: Ponto -> Double
angulo (Cartesiano x y )= atan(y/x)

--e)

polarToCart ::Ponto ->Ponto
polarToCart (Polar a b)= (Cartesiano (posx (Polar a b)) (posy (Polar a b))) 

dist2 :: Ponto -> Ponto -> Double
dist2 (Cartesiano x y) (Cartesiano a b) = dist (x,y) (a,b)
dist2 (Cartesiano x y) (Polar a b) = dist2 (Cartesiano x y) (polarToCart (Polar a b))
dist2 (Polar a b) (Polar x y) = dist2 (polarToCart (Polar a b)) (polarToCart (Polar a b))

--7)

data Figura = Circulo Ponto Double | Rectangulo Ponto Ponto | Triangulo Ponto Ponto Ponto deriving (Show,Eq)

--a

poligono :: Figura -> Bool
poligono (Circulo a b )= False
poligono (Rectangulo x y )= True
poligono (Triangulo a b c) = True

--b)

vertices :: Figura -> [Ponto] 
vertices (Circulo a b) = []
--vertices (Rectangulo (Ponto a b) (Ponto x y)) = [(Ponto a b)]
vertices (Triangulo a b c)=[a]++[b]++[c]

--c)

--area :: Figura -> Double
--area (Triangulo p1 p2 p3) = let a = dist p1 p2 
--                            b = dist p2 p3
--                            c = dist p3 p1
--s = (a+b+c) / 2 in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron

--d)

perimetro2 :: Figura -> Double
perimetro2 (Circulo p d)=2*pi*d
--perimetro (Rectangulo a b)=dist a b
perimetro2 (Triangulo a b c)= (dist2 a b) +( dist2 b c )+ (dist2 c a)

