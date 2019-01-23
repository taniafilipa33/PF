import Data.Char
--Ficha 3



--1

data Hora = H Int Int deriving Show
type Etapa = (Hora,Hora)
type Viagem = [Etapa]

--a) 

horaValida :: Hora -> Bool
horaValida (H a b)| a>=0 && a>24 && b>=0 && b<60 = True
                  | otherwise = False

horaDepois :: Hora -> Hora -> Bool
horaDepois (H a b) (H x y)|a<b =True
                          |a==b && y>b = True
                          |otherwise = False

etapaCerta :: Etapa -> Bool
etapaCerta  (a,b) | horaValida a == True && horaValida b == True && horaDepois a b == True = True
                  |otherwise = False                          

--b)


viagemCerta :: Viagem -> Bool
viagemCerta [] = True
viagemCerta (l:ls) | etapaCerta l == True  = True 
                   |otherwise = False


--c)

daHora :: Etapa -> Hora
daHora (a,b) = b

horaPartida :: Viagem -> Hora
horaPartida (l:ls) = daHora l 

chegHora :: Etapa -> Hora
chegHora (a,b) = b

horaChegada :: Viagem -> Hora
horaChegada  ls = chegHora (last ls)

--d)

--daTempo:: Hora -> Hora -> Double

--2)
data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)

type Poligonal = [Ponto]


posx :: Ponto -> Double
posx (Polar x y )= x * cos y
posx (Cartesiano x y )= x

--b)

posy :: Ponto -> Double
posy (Polar x y )= x * sin y
posy (Cartesiano x y )= y

dist :: (Double,Double) -> (Double,Double)-> Double
dist (a,b) (x,y) = sqrt(((x - a)^2) + ((y - b)^2))

polarToCart ::Ponto ->Ponto
polarToCart (Polar a b)= (Cartesiano (posx (Polar a b)) (posy (Polar a b))) 

dist2 :: Ponto -> Ponto -> Double
dist2 (Cartesiano x y) (Cartesiano a b) = dist (x,y) (a,b)
dist2 (Cartesiano x y) (Polar a b) = dist2 (Cartesiano x y) (polarToCart (Polar a b))
dist2 (Polar a b) (Polar x y) = dist2 (polarToCart (Polar a b)) (polarToCart (Polar a b))


--a)

comp :: Poligonal -> Double
comp [] = 0
comp (l:ls) = (dist2 l (head ls))+comp (tail ls)

--b)
isCardinal:: Ponto -> Bool
isCardinal (Cartesiano a b) = True
isCardinal (Polar a b) = False

isFechado :: Poligonal -> Bool
isFechado [] = False
isFechado l | isCardinal (head l) == False && isCardinal (last l)== True && polarToCart(head l) == last l =True
            | head l == last l =True
            | isCardinal (head l) == True && isCardinal (last l)== False && head l == polarToCart(last l) =True
            |otherwise = False


--c)

--triangula :: Poligonal -> [Figura]
--LMao I GUESS

--d)

--e)
transformaSeNec :: Ponto -> Ponto
transformaSeNec a | isCardinal a == True = a
                  |otherwise = polarToCart a
somaPontosCart :: Ponto -> Ponto -> Ponto
somaPontosCart (Cartesiano a b) (Cartesiano x y) = (Cartesiano (a+x) (b+y)) 

somaPontos :: Ponto -> Ponto -> Ponto
somaPontos a b = somaPontosCart (transformaSeNec a) (transformaSeNec b)

mover :: Poligonal -> Ponto -> Poligonal
mover [] _ = []
mover (l:ls) x = (somaPontos l x) : (mover ls x)
 
--f)
somaDouble :: Double -> Ponto -> Ponto
somaDouble x (Cartesiano a b) = (Cartesiano (a*x) (b*x))  

zoom :: Double -> Poligonal -> Poligonal
zoom _ [] = []
zoom x (l:ls)= l: (zoom x (( somaDouble x (transformaSeNec (head ls))):(tail ls)))

--crlh sinto me inteligente ksdksskd


--3)

data Contacto = Casa Integer | Trab Integer | Tlm Integer | Email String deriving Show
type Nome1 = String
type Agenda = [(Nome1, [Contacto])]

--a)

acrescEmail :: Nome1 -> String -> Agenda -> Agenda
acrescEmail n s [] = [(n,[Email s])]
acrescEmail n s l= (n, [Email s]):l

--b)

verEmails :: Nome1 -> Agenda -> Maybe [String]
verEmails _ [] = Nothing
verEmails n ((name,l):ls) | n==name = Just (procuraMails l )
                   |otherwise = verEmails n ls where 
                   procuraMails [] = []
                   procuraMails ((Email s):ts)=s:procuraMails ts
                   prucuraMails (_:ts)=procuraMails ts
                   
--c)

consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs ((Tlm n):ls)= n:(consTelefs ls)
consTelefs ((Casa n):ls)= n:(consTelefs ls)
consTelefs ((Trab n):ls)= n:(consTelefs ls)
consTelefs (_:ls)= consTelefs ls

--d)

casa :: Nome1 -> Agenda -> Maybe Integer
casa _ [] = Nothing
casa n ((name,list):ls)|n==name = daCasa list
                       |otherwise = casa n ls where
                       daCasa [] = Nothing
                       daCasa ((Casa n):ls)= Just n
                       daCasa (_:ls)=daCasa ls

--4

type Dia = Int
type Mes = Int
type Ano = Int
type Nome = String
data Data = D Dia Mes Ano deriving Show
type TabDN = [(Nome,Data)]

--a) 

procura :: Nome -> TabDN -> Maybe Data
procura n [] = Nothing
procura n ((name,dat):ls)|n==name = Just dat
                          |otherwise = procura n ls 

--b)

idade :: Data -> Nome -> TabDN -> Maybe Int
idade d n [] = Nothing
idade d n ((name, dat):ls) | n == name = (daData d dat)
                           | otherwise = idade d n ls where
                           daData :: Data -> Data -> Maybe Int
                           daData (D d m a) (D i e n) | a>n && m<e = Just (a-n)
                                                      | a>n && m>e =Just (a-n-1)
                                                      | a>n && m==e && d>=i = Just (a-n)
                                                      | a>n && m==e && d<i = Just (a-n-1)
                                                      | a==n = Just 0
                                                      |otherwise = Nothing


--c)

anterior :: Data -> Data -> Bool
anterior (D d m a) (D i e n) | a<n =True
                             | a==n && m<e= True
                             | a==n && m==e && d<i = True
                             |otherwise = False


--d)

daPeq:: TabDN -> (Nome,Data)
daPeq [] = undefined
daPeq [(n,d)] = (n,d)
daPeq (l:ls)|anterior (snd l) (snd (head ls))== True = daPeq(l:(tail ls)) 
            |otherwise = daPeq ls

tira :: (Nome,Data) -> TabDN -> TabDN
tira d [] = []
tira (n,d) ((nome,dat):ls) | n == nome = ls
                           |otherwise = (nome,dat):(tira (n,d) ls) 

ordena :: TabDN -> TabDN
ordena [] = []
ordena l = (daPeq (l)):(ordena((tira (daPeq l) l )))

--e)

--porIdade:: Data -> TabDN -> [(Nome,Int)]
--foi usada a função map, mas não entendi como foi feita o resto da função

--5

data Movimento = Credito Float | Debito Float deriving Show
data Data2 = D2 Int Int Int deriving Show
data Extracto = Ext Float [(Data, String, Movimento)] deriving Show

--a)
credmaior :: Movimento -> Float -> Bool
credmaior (Credito c) x | c>x = True
                        |otherwise = False
credmaior (Debito c) x  | c>x = True
                        |otherwise = False


extValor :: Extracto -> Float -> [Movimento] 
extValor (Ext a []) _ = []
extValor (Ext a ((d,s,m):ls)) x| (credmaior m x) == True = m:(extValor (Ext a ls) x)
                       |otherwise = extValor (Ext a ls) x

--b)

filtro :: Extracto -> [String] -> [(Data,Movimento)]
filtro (Ext a []) _ = []
filtro _ [] = []
filtro (Ext a ((d,s,m):ls)) l| estaLista s l == True = (d,m):(filtro (Ext a ls) l)
                             |otherwise = (filtro (Ext a ls) l) where
                             estaLista :: String -> [String] -> Bool
                             estaLista _ [] = False
                             estaLista s (l:ls)| s==l = True
                                               |otherwise = estaLista s ls

--c)

somaD:: (Float,Float)-> (Float,Float) -> (Float,Float)
somaD (a,b) (x,y)= (a+x,y+b)

creDeb :: Extracto -> (Float,Float)
creDeb (Ext _ []) = (0,0)
creDeb (Ext l ((d,s,Credito a):ls))= somaD (a,0) (creDeb (Ext l ls))  
creDeb (Ext l ((d,s,Debito  a):ls))= somaD (0,a) (creDeb (Ext l ls))                                               

--d)

saldo :: Extracto -> Float
saldo (Ext _ []) = 0
saldo (Ext l ((d,s,Credito a):ls)) = (l+a)+(saldo (Ext (l+a) ls))
saldo (Ext l ((d,s,Debito  a):ls)) = (l-a)+(saldo (Ext (l-a )ls))