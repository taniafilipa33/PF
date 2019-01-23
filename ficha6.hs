--ficha 6

import Data.Char

data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show

--1)

--a)

altura :: BTree a -> Int
altura Empty = 0
altura (Node x a b)  | altura a >= altura b = 1+altura a
                     |otherwise = 1+altura b

--b)

contaNodos :: BTree a -> Int                  
contaNodos Empty = 0
contaNodos (Node x a b) = 1+(contaNodos a )+ (contaNodos b)

--c)

folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node _ Empty Empty) = 1
folhas (Node _ x y) = (folhas x)+(folhas y) 

--d)

prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune 0 l = Empty
prune x (Node c a b ) =(Node c (prune (x-1) a) (prune (x-1) b))

--e)

path :: [Bool] -> BTree a -> [a]
path [] (Node a b c) = [a]
path _ Empty = []
path (True:ls) (Node a b c)=a:(path ls (c))
path (False:ls) (Node a b c)=a:(path ls (b))

--f)

mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node a b c) = Node a (mirror c) (mirror b)

--g)

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node x xleft xright) (Node y yleft yright) = Node (f x y) (zipWithBT f xleft yleft) (zipWithBT f xright yright)
zipWithBT _ _ _ = Empty

--h)

unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty,Empty,Empty)
unzipBT (Node (x,y,z) lnode rnode) = (Node x al ar, Node y bl br, Node z cl cr) where
    (al, bl, cl) = unzipBT lnode
    (ar, br, cr) = unzipBT rnode

--2)

--a)

minimo :: Ord a => BTree a -> a
minimo (Node a Empty Empty) = a
minimo (Node a (Node b x s) Empty) =  if a>=b then minimo (Node b x s) else minimo (Node a x s)
minimo (Node a Empty (Node b x s)) =  if a>=b then minimo (Node b x s) else minimo (Node a x s)
minimo (Node a b c) = daMin a b c where
    daMin :: Ord a => a -> BTree a -> BTree a -> a
    daMin a Empty Empty = a
    daMin a (Node b x s) Empty =  if a>=b then daMin b x s else daMin a x s
    daMin a Empty (Node b x s)=  if a>=b then daMin b x s else daMin a x s
    daMin a (Node b n1 n2) (Node c n3 n4)| a>=b && a>=c = if ((daMin b n1 n2)<=(daMin c n3 n4)) then (daMin b n1 n2) else (daMin c n3 n4)
                                         | a<b && a<c = if ((daMin a n1 n2)<=(daMin a n3 n4)) then (daMin a n1 n2) else (daMin a n3 n4)
                                         | a>=b && a<c = if ((daMin b n1 n2)<=(daMin a n3 n4)) then (daMin b n1 n2) else (daMin a n3 n4)
                                         | a<b && a>=c = if ((daMin a n1 n2)<=(daMin c n3 n4)) then (daMin a n1 n2) else (daMin c n3 n4)

--b)

semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node a Empty Empty) = Empty
--semMinimo (Node a (Node c v r) b) | a==minimo (node a (Node c v r) b) = Node c (Node )

--3)

type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int | Rep| Faltou deriving Show
type Turma = BTree Aluno -- ´arvore bin´aria de procura (ordenada por n´umero)

--testes ::: EXplo :: mediaAprov (Node (7,"n",ORD,Rep) (Node (5,"u",TE,Rep) (Node (4,"v",ORD,Rep) Empty Empty) Empty) (Node (1,"b",ORD,Rep) (Node (66,"t",TE,Aprov 12) Empty Empty) (Node (6,"y",TE,Aprov 10) Empty Empty)))


--a)

inscNum :: Numero -> Turma -> Bool
inscNum _ Empty = False
inscNum x (Node (a,_,_,_) b c)| x==(a) = True
                              | inscNum x b == True = True
                              |otherwise = inscNum x c 

--b)

inscNome :: Nome -> Turma -> Bool
inscNome _ Empty = False
inscNome nome (Node (_,nam,_,_) c b) |nome == nam = True
                                     |inscNome nome c == True = True
                                     | otherwise = inscNome nome b

--c)

trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (num,name,TE,_) a b) = [(num,name)]++(trabEst a)++(trabEst b)
trabEst (Node (num,name,_,_) a b) = (trabEst a)++(trabEst b)                              

--d)

nota ::  Numero -> Turma -> Maybe Classificacao
nota num Empty = Nothing
nota num (Node (n,t,v,x) b s) | inscNum num (Node (n,t,v,x) b s)== False = Nothing
                              | num==n = Just x
                              | num/=n && inscNum num b== True = nota num b
                              | num/=n && inscNum num s== True = nota num s
                              |otherwise = Nothing


--e)

percFaltas :: Turma -> Float                              
percFaltas Empty = undefined
percFaltas l = ((nFaltas l)/(nLength l))*100 where
    nLength :: Turma -> Float
    nLength Empty = 0
    nLength (Node x b c)= 1+(nLength b)+(nLength c) 
    nFaltas :: Turma -> Float
    nFaltas Empty = 0
    nFaltas (Node (_,_,_,Faltou) a b)= 1+(nFaltas a)+(nFaltas b)
    nFaltas (Node (_,_,_,_) a b)=(nFaltas a)+(nFaltas b)

--f)

mediaAprov :: Turma -> Float
mediaAprov Empty = 0
mediaAprov l = (somaNotas l)/(nAprov l) where 
    somaNotas :: Turma -> Float
    somaNotas Empty = 0
    somaNotas (Node (_,_,_,Aprov a) b c) = (fromIntegral(a)) + (somaNotas b) + (somaNotas c)
    somaNotas (Node _ b c) = (somaNotas b) + (somaNotas c)
    nAprov :: Turma -> Float
    nAprov Empty = 0
    nAprov (Node (_,_,_,Aprov _) a b) = 1+ (nAprov a)+(nAprov b)
    nAprov (Node _ a b)= (nAprov a)+(nAprov b)

--g)

aprovAv :: Turma -> Float
aprovAv t = let (a,b) = aAAux t in a / b where
    aAAux Empty = (0,0)
    aAAux (Node (_, _, _, Aprov x) lnode rnode) = (1 + al + ar, 1 + bl + br) where
        (al,bl) = aAAux lnode
        (ar,br) = aAAux rnode
    aAAux (Node (_, _, _, Rep) lnode rnode) = (0 + al + ar, 1 + bl + br) where
        (al,bl) = aAAux lnode
        (ar,br) = aAAux rnode
    aAAux (Node _ lnode rnode) = (0 + al + ar, 0 + bl + br) where
        (al,bl) = aAAux lnode
        (ar,br) = aAAux rnode