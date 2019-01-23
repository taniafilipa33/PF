--ficha.7
import Data.Char

--1)


data ExpInt = Const Int | Simetrico ExpInt | Mais ExpInt ExpInt | Menos ExpInt ExpInt | Mult ExpInt ExpInt

--a)

calcula :: ExpInt -> Int
calcula (Const a) = a
calcula (Simetrico a) = - (calcula a)
calcula (Mais a b) = (calcula a) + ( calcula b)
calcula (Menos a b)= (calcula a) - ( calcula b)
calcula (Mult a b) = (calcula a) * ( calcula b)

--b)

infixa :: ExpInt -> String
infixa (Const a) = [intToDigit(a)]
infixa (Simetrico a) = ['-']++( infixa a)
infixa (Mais a b) =['(']++(infixa a)++['+']++(infixa b)++[')']
infixa (Menos a b) =['(']++(infixa a)++['-']++(infixa b)++[')']
infixa (Mult a b) =['(']++(infixa a)++['*']++(infixa b)++[')']

--c)
--posso adicionar espaços se quiser
posfixa :: ExpInt -> String
posfixa (Const a) = [intToDigit(a)]
posfixa (Simetrico a) = ['-']++( posfixa a)
posfixa (Mais a b) =(posfixa a)++(posfixa b)++['+']
posfixa (Menos a b) =(posfixa a)++(posfixa b)++['-']
posfixa (Mult a b) =(posfixa a)++(posfixa b)++['*']

--2)

data RTree a = R a [RTree a]

--a)

soma :: Num a => RTree a -> a
soma (R a []) = a
soma (R a l) = a + (sum (map soma l))

--b)

altura :: RTree a -> Int
altura (R a []) = 1
altura (R a l) = 1 + (sum (map altura l))

--c)

prune :: Int -> RTree a -> RTree a
prune _ (R a []) = (R a [])
prune 0 (R a l) = (R a [])
prune x (R a ls) = (R a (map (prune (x-1)) ls))

--d)

mirror :: RTree a -> RTree a
mirror (R a []) = (R a [])
mirror (R a l) = (R a (map mirror (reverse l )))

--e)

postorder :: RTree a -> [a]
postorder (R v subNodes) = foldr (++) [v] (map postorder subNodes)

--3)

data BTree a = Empty | Node a (BTree a) (BTree a)
data LTree a = Tip a | Fork (LTree a) (LTree a)

--a)

ltSum :: Num a => LTree a -> a
ltSum (Tip a) = a
ltSum (Fork a b) = (ltSum a)+(ltSum b)

--b)

listaLT :: LTree a -> [a]
listaLT (Tip a) = [a]
listaLT (Fork a b) = (listaLT a) ++ (listaLT b)

--c)

ltHeight :: LTree a -> Int
ltHeight (Tip _) = 1
ltHeight (Fork a b) = 1+ (max(ltHeight a) (ltHeight b))

--4)

data FTree a b = Leaf b | No a (FTree a b) (FTree a b)

--a)

splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf b) = (Empty,(Tip b))
splitFTree (No x l r) = let (l1, l2) = splitFTree l
                            (r1, r2) = splitFTree r in (Node x l1 r1, Fork l2 r2)

--b)

joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTress Empty (Tip a)= Just (Leaf a)
joinTrees (Node x e d) (Fork e' d') = case (joinTrees e e') of
    Nothing -> Nothing
    Just e1 -> case (joinTrees d d') of
        Nothing -> Nothing
        Just d1 -> Just (No x e1 d1)
joinTrees _ _ = Nothing