module Ficha8 where

import Data.Char

-- Exercício 1
data Frac = F Integer Integer

-- a)
mdc :: Integer -> Integer -> Integer
mdc x 0 = x
mdc x y = mdc y (x `mod` y)

normaliza :: Frac -> Frac
normaliza (F x y) = F x' y'
    where sinal = if (x * y > 0) then 1 else -1
          m = mdc (abs x) (abs y)
          x' = sinal * ((abs x) `div` m)
          y' = (abs y) `div` m

-- b)
instance Eq Frac where
    (==) (F x y) (F x' y') = x * y' == y * x'

-- c)
instance Ord Frac where
    (<=) f f' = x * w <= y * z
        where (F x y) = normaliza f
              (F z w) = normaliza f'

-- d)
instance Show Frac where
    show f = show z ++ "/" ++ show w
        where (F z w) = normaliza f

-- e)
instance Num Frac where
    (+) (F x y) (F z w) = normaliza (F (x * w + y * z) (y * w))
    (*) (F x y) (F z w) = normaliza (F (x * z) (y * w))
    abs (F x y) = normaliza (F (abs x) (abs y))
    signum f
        | x < 0 = -1
        | otherwise = 1
        where (F x y) = normaliza f
    fromInteger x = F x 1
    negate f = (F (x * (-1)) y)
        where (F x y) = normaliza f

-- f)
maioresQueDobro :: Frac -> [Frac] -> [Frac]
maioresQueDobro _ [] = []
maioresQueDobro f l = filter (\elem -> (normaliza elem) > (2 * f')) l
    where f' = normaliza f

-- Exercício 2
data Exp a = Const a
           | Simetrico (Exp a)
           | Mais (Exp a) (Exp a)
           | Menos (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)

-- a)
instance Show a => Show (Exp a) where
    show (Const x) = show x
    show (Simetrico x) = "(-" ++ show x ++ ")"
    show (Mais x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
    show (Menos x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
    show (Mult x y) = "(" ++ show x ++ " * " ++ show y ++ ")"

-- b)
instance (Num a, Eq a) => Eq (Exp a) where
    (==) exp exp' = (calcula exp) == (calcula exp')

calcula :: Num a => Exp a -> a
calcula exp =
    case exp of
        (Const x) -> x
        (Simetrico x) -> - (calcula x)
        (Mais x y) -> (calcula x) + (calcula y)
        (Menos x y) -> (calcula x) - (calcula y)
        (Mult x y) -> (calcula x) * (calcula y)

-- c)
instance (Num a, Eq a) => Num (Exp a) where
    (+) x y = Const (calcula x + calcula y)
    (-) x y = Const (calcula x - calcula y)
    (*) x y = Const (calcula x * calcula y)
    abs exp =
        case exp of
            (Const x) -> Const (abs x)
            (Simetrico x) -> abs x
            (Mais x y) -> abs (x + y)
            (Menos x y) -> abs (x - y)
            (Mult x y) -> abs (x * y)
    signum exp =
        case exp of
            (Const x)
                | abs x == x -> 0
                | abs x == x && x == 0 -> 1
                | otherwise -> -1
            (Simetrico x) -> - signum x
            (Mais x y)
                | abs (x + y) == x + y && x + y == 0 -> 0
                | abs (x + y) == x + y -> 1
                | otherwise -> 1
            (Menos x y)
                | abs (x - y) == x - y && x - y == 0 -> 0
                | abs (x - y) == x - y -> 1
                | otherwise -> 1
            (Mult x y)
                | abs (x * y) == x * y && x * y == 0 -> 0
                | abs (x * y) == x * y -> 1
                | otherwise -> 1
    fromInteger x = Const (fromInteger x)

-- Exercício 3
data Movimento = Credito Float | Debito Float
data Data = D Int Int Int deriving Eq
data Extracto = Ext Float [(Data, String, Movimento)]

-- a)
instance Ord Data where
    compare (D d m a) (D d' m' a')
        | a > a' || a == a' && (m > m' || m == m' && d > d') = GT
        | a == a' && m == m' && d == d' = EQ
        | otherwise = LT

-- b)
instance Show Data where
    show (D d m a) = show d ++ "/" ++ show m ++ "/" ++ show a

-- c)
ordena :: Extracto -> Extracto
ordena (Ext x []) = (Ext x [])
ordena (Ext x (h:t)) = (Ext x (menores ++ [h] ++ maiores))
    where menores = [x | x <- t, fst' x < fst' h]
          maiores = [x | x <- t, fst' x >= fst' h]
          fst' (d,_,_) = d

-- d)
instance Show Extracto where
    show ext@(Ext x l) =
        "Saldo anterior: " ++ show x ++
        "\n---------------------------------------" ++
        "\nData     Descricao   Credito   Debito  " ++
        "\n---------------------------------------\n" ++ "" ++ -- Falta definir isto
        "-----------------------------------------" ++
        "\nSaldo actual: " ++ show (saldo ext)

saldo :: Extracto -> Float
saldo (Ext x []) = x
saldo (Ext x l) = foldr (\(_,_,mov) acc ->
    case mov of
        Credito val -> acc + val
        Debito val -> acc - val) x l
