module Ficha2 where

import Data.Char
import Data.List
import Data.Function

-- Exercício 1

-- a)
funA :: [Double] -> Double
funA [] = 0
funA (y:ys) = y^2 + (funA ys)

{- funA [2,3,5,1]
   2 ^ 2 + (funA [3,5,1])
   4 + 3 ^ 2 + (funA [5,1])
   4 + 9 + 5 ^ 2 + (funA [1])
   4 + 9 + 25 + 1 ^ 2 + (funA [])
   4 + 9 + 25 + 1 + 0
   39
-}

-- b)
funB :: [Int] -> [Int]
funB [] = []
funB (h:t) = if (mod h 2) == 0 then h : (funB t)
                               else (funB t)
{- funB [8,5,12]
   8 : (funB [5,12])
   8 : (funB [12])
   8 : 12 : (funB [])
   8 : 12 : []
   [8,12]
-}

-- c)
funC (x:y:t) = funC t
funC [x] = [x]
funC [] = []

{- funC [1,2,3,4,5]
   funC [3,4,5]
   funC [5]
   [5]
-}

-- d)
funD l = g [] l
g acc [] = acc
g acc (h:t) = g (h:acc) t

{- funD "otrec"
   g [] "otrec"
   g 'o' : [] "trec"
   g 't' : 'o' "rec"
   g 'r' : 't' : 'o' "ec"
   g 'e' : 'r' : 't' : 'o' "c"
   g 'c' : 'e' : 'r' : 't' : 'o' []
   "certo"
-}

-- Exercício 2

-- a)
dobros :: [Float] -> [Float]
dobros l = map (\elem -> 2 * elem) l

-- b)
numOcorre :: Char -> String -> Int
numOcorre c l = length $ filter (\elem -> elem == c) l

-- c)
positivos :: [Int] -> Bool
positivos l = all (\elem -> elem >= 0) l

-- d)
soPos :: [Int] -> [Int]
soPos l = filter (\elem -> elem >= 0) l

-- e)
somaNeg :: [Int] -> Int
somaNeg l = sum $ filter (\elem -> elem < 0) l

-- f)
tresUlt :: [a] -> [a]
tresUlt l@(h:t)
    | length l <= 3 = l
    | otherwise = tresUlt t

-- g)
segundos :: [(a,b)] -> [b]
segundos l = map (snd) l

-- h)
nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros x l = x `elem` primeiros
    where primeiros = map (fst) l

-- i)
sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [(a,b,c)] = (a,b,c)
sumTriplos ((a,b,c):t) = (a + x, b + y, c + z)
    where (x,y,z) = sumTriplos t

-- Exercício 3

-- a)
soDigitos :: [Char] -> [Char]
soDigitos l = filter (\elem -> elem >= '0' && elem <= '9') l

-- b)
minusculas :: [Char] -> Int
minusculas l = length $ filter (\elem -> elem >= 'a' && elem <= 'z') l

-- c)
nums :: String -> [Int]
nums l = map (\elem -> (ord $ elem) - 48) (soDigitos l)

-- Exercício 4
type Polinomio = [Monomio]
type Monomio = (Float, Int)

-- a)
conta :: Int -> Polinomio -> Int
conta grau l = length $ filter (\elem -> grau == snd elem) l

-- b)
grau :: Polinomio -> Int
grau l = maximum $ map snd l

-- c)
selgrau :: Int -> Polinomio -> Polinomio
selgrau grau l = filter (\elem -> grau == snd elem) l

-- d)
deriv :: Polinomio -> Polinomio
deriv l = map (\elem -> ((fromIntegral $ snd elem) * fst elem, (snd elem) - 1)) l

-- e)
calcula :: Float -> Polinomio -> Float
calcula x l = foldr (\elem elem' -> (fst elem * (x ^ snd elem)) + elem') 0 l 

-- f)
simp :: Polinomio -> Polinomio
simp l = filter (\elem -> fst elem /= 0) l

-- g)
mult :: Monomio -> Polinomio -> Polinomio
mult (f,s) l = map (\elem -> (fst elem * f, snd elem + s)) l

-- h)
normaliza :: Polinomio -> Polinomio
normaliza l = normaliza' $ juntaPorExpoente l
    where juntaPorExpoente :: Polinomio -> [Polinomio]
          juntaPorExpoente l = groupBy (\elem elem' -> snd elem == snd elem') l'
          l' = sortBy (compare `on` snd) l

normaliza' :: [Polinomio] -> Polinomio
normaliza' [] = []
normaliza' [[x]] = [x]
normaliza' l@(h:t) = (foldr (+) 0 (primeiros h), snd $ head h) : normaliza' t
    where primeiros l = map (fst) l

-- i)
soma :: Polinomio -> Polinomio -> Polinomio
soma l l' = normaliza (l ++ l')

-- j)
produto :: Polinomio -> Polinomio -> Polinomio
produto (h:t) l = soma (mult h l) (produto t l)

-- k)
ordena :: Polinomio -> Polinomio
ordena l = sortBy (compare `on` snd) l

-- l)
equiv :: Polinomio -> Polinomio -> Bool
equiv l l' = (ordena $ normaliza l) == (ordena $ normaliza l')
