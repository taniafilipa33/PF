import Data.Char
--teste
--1


myinsert :: Ord a => a -> [a] -> [a]
myindert a [] = [a]
myinsert a (l:ls) | a<=l = [a]++(l:ls)
                  | a>l  = l:(myinsert a ls)


--2)

mycatMaybes :: [Maybe a] -> [a]
mycatMaybes [] = []
mycatMaybes (Nothing:ls) = mycatMaybes ls
mycatMaybes ((Just a):ls)= a:(mycatMaybes ls)

--3)



data Exp a = Const a | Var String | Mais (Exp a) (Exp a) | Mult (Exp a) (Exp a)
 
instance Show a => Show (Exp a) where
    show (Const a) = show a
    show (Var n)=  n
    show (Mais a b)= "("++show a++"+"++show b++")"
    show (Mult a b)="("++show a++"*"++show b++")"

--4)

--sortOn :: Ord b => (a -> b) -> [a] -> [a]
--sortOn _ [] = []
--sortOn a (l:ls) = (a l ):(sortOn a ls)

--5)

amplitude :: [Int] -> Int
amplitude [] = 0
amplitude l =(max1 l)-(min1 l) where 
    max1 :: [Int] -> Int
    max1 [x] = x
    max1 (l:ls)|l<=(head ls) =max1 ls
               |otherwise   =max1 (l:(tail ls))
    min1 :: [Int] -> Int
    min1 [s] = s
    min1 (l:ls)|l>=(head ls) =min1 ls
               |otherwise   =min1 (l:(tail ls))             

--6)

--6)
data Imagem = Quadrado Int | Mover (Int,Int) Imagem | Juntar [Imagem]

--a)

conta :: Imagem -> Int
conta (Quadrado a) = 1
conta (Mover a b)  = conta b
conta (Juntar (l:ls)) = (conta l) + (conta  (Juntar ls))

--b)

--EXAME


--1))
mymymy :: [a] -> Int -> a
mymymy [] _ = undefined
mymymy (l:ls) x | x== 0 = l
                |otherwise = mymymy ls (x-1)

--2))

data Movimento = Norte | Sul | Este | Oeste deriving Show

posicao :: (Int, Int) -> [Movimento] -> (Int, Int)
posicao a [] = a
posicao (x,y) (Norte:ls) = posicao (x,y-1) ls
posicao (x,y) (Sul:ls) = posicao (x,y+1) ls
posicao (x,y) (Este:ls) = posicao (x+1,y) ls
posicao (x,y) (Oeste:ls) = posicao (x-1,y) ls

--3

myany :: (a -> Bool) -> [a] -> Bool
myany x [] = False
myany x (l:ls) = if x l == True then True else myany x ls

--4

type Mat a = [[a]]

daate ::(Num a, Eq a)=> Int -> [a] -> Bool
daate _ [] = False
daate 0 (l:ls) = if l==0 then True else False
daate x (l:ls) | l==0 && x>0 = daate (x-1) ls
               |otherwise = False 

triSup :: (Num a, Eq a)  => Mat a -> Bool
triSup [[]] = False
triSup [x] = False
triSup ((l:ls):xs) = if trisuppy 0 xs == True then True else False where
    trisuppy ::(Num a, Eq a) => Int -> Mat a -> Bool
    trisuppy x [[]] = False
    trisuppy x [l] = daate x l
    trisuppy a (l:ls) | daate a l == True = trisuppy (a+1) ls
                      |otherwise = False 

--5)                      

--movimenta :: IO (Int,Int)
--movimenta = move (0,0) 
--move:: (Int,Int) -> IO (Int,Int)
--move (x,y) = do
--    dir <- getChar
--    if dir=='N' then move (x,y-1)
--    else if dir=='S' then move (x,y+1)
--    else if dir=='E' then move (x+1,y)
--    else if dir=='O' then move (x-1,y)
--    else return (x,y) 

--6)

--eXAME 2016

--1)
--a)


myunlines :: [String] -> String
myunlines [l] = l++" "
myunlines (l:ls)= l ++ "\n" ++ myunlines ls

--b
mybarras :: (Eq a) => [a] -> [a] -> [a]
mybarras [] _ = []
mybarras a [] = a
mybarras l (x:xs)| elem x l== True = mybarras (remove x l) xs
                 |otherwise = mybarras l xs where 
                    remove::(Eq a) => a -> [a] -> [a]
                    remove _ [] = []
                    remove a (l:ls) | a== l = ls
                                    |otherwise = l:(remove a ls)

-- 2

data Seq a = Nil | Inicio a (Seq a) | Fim (Seq a) a

--a)

primeiro :: Seq a -> a
primeiro (Inicio a b) = a
primeiro (Fim a b)=primeiro a                                   

--b)

semUltimo :: Seq a -> Seq a
semUltimo (Nil) = Nil
semUltimo ( Inicio a b) = Inicio a (semUltimo b)
semUltimo (Fim a b)= a

--3))

data BTree a = Empty | Node a (BTree a) (BTree a)

--a)

prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune 0 (Node a b c) = Node a Empty Empty
prune x (Node a b c) = Node a (prune (x-1) b) (prune (x-1) c)

--b)

--semMinimo :: (Ord a) => BTree a -> BTree a
--semMinimo Empty = Empty
--semMinimo (Node a Empty Empty) = Empty
--semMinimo a = remocee (mini a) a where
--    remocee :: (Ord a)=> a -> Btree a -> Btree a
--    remocee _ Empty = Empty
--    remocee a (Node b c d) | a==b 

semMinimo :: (Ord a) => BTree a -> BTree a
semMinimo Empty = Empty
semMinimo (Node _ Empty r) = r
semMinimo (Node x l r) = Node x (semMinimo l) r