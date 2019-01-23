--ficha8.hs
--1

data Frac = F Integer Integer

--a)
mdc :: Integer -> Integer -> Integer
mdc a b | a> b = mdc (a-b) b
        |a<b   = mdc a (b-a)
        |a==b  = a

normaliza :: Frac -> Frac
normaliza (F x 0) = error "denominador nulo"
normaliza (F 0 x) = F 0 1
normaliza (F n d) = F ((signum d) * (n `div` m)) ((abs d) `div` m) where m = mdc (abs n) (abs d)

--b)

instance Eq Frac where -- ver "slides 06" para a definição da classe.
    (==) x y = (a1 == a2) && (b1 == b2) where
        (F a1 b1) = normaliza x
        (F a2 b2) = normaliza y

--c)

instance Ord Frac where -- ver "slides 06" para a definição da classe.
    compare (F n1 d1) (F n2 d2) = compare (n1 * d2) (d1 * n2)
-- ou	(<=) (F n1 d1) (F n2 d2) = (n1 * d2) <= (d1 * n2)


--d)

instance Show Frac where -- ver "slides 06" para a definição da classe.
    show (F num den) = "(" ++ show num ++ "/" ++ show den ++ ")"

--e)

instance Num Frac where
    (+) (F n1 d1) (F n2 d2) = normaliza (F ((n1 * d2) + (n2 * d1)) (d1 * d2))
    (*) (F n1 d1) (F n2 d2) = normaliza (F (n1 * n2) (d1 * d2))
    (-) (F n1 d1) (F n2 d2) = normaliza (F ((n1 * d2) - (n2 * d1)) (d1 * d2))
    abs (F n d) = (F (abs n) (abs d))
    signum (F n d) = let (F a b) = normaliza (F n d) in if (a == 0) then 0 else if (a > 0) then 1 else (-1)
    fromInteger x = (F x 1)

--f)

twiceBiggerThen :: Frac -> [Frac] -> [Frac]
twiceBiggerThen _ [] = []
twiceBiggerThen f (h:ts) = if (h > (2 * f)) then h : (twiceBiggerThen f ts) else (twiceBiggerThen f ts)

--2)

data Exp a = Const a | Simetrico (Exp a) | Mais (Exp a) (Exp a) | Menos (Exp a) (Exp a) | Mult (Exp a) (Exp a)

--a)

instance Show Exp a where
    show (Const a) = show a