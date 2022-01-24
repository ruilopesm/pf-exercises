module Ficha5 where

import Prelude hiding (any, zipWith, takeWhile, dropWhile, span, deleteBy)
import Data.List hiding (any, zipWith, takeWhile, dropWhile, span, deleteBy, transpose)
import Data.Function

-- Exercício 1

-- a)
any :: (a -> Bool) -> [a] -> Bool
any f [x] = f x
any f (h:t)
    | f h = True
    | otherwise = any f t

-- b)
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (h:t) (h':t') = f h h' : zipWith f t t'

-- c)
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile f (h:t)
    | f h = h : takeWhile f t
    | otherwise = []

-- d)
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile f l@(h:t)
    | f h = dropWhile f t
    | otherwise = l

-- e)
span :: (a -> Bool) -> [a] -> ([a],[a])
span _ [] = ([],[])
span f l@(h:t)
    | f h = (h : x,y)
    | otherwise = ([],l)
    where (x,y) = span f t

-- f)
deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy _ _ [] = []
deleteBy f x (h:t)
    | f x h = t
    | otherwise = h : deleteBy f x t

-- g)
-- Usa quick sort
sortOnQuick :: Ord b => (a -> b) -> [a] -> [a]
sortOnQuick _ [] = []
sortOnQuick f (h:t) = menores ++ [h] ++ maiores
    where menores = sortOnQuick f [x | x <- t, f x < f h]
          maiores = sortOnQuick f [x | x <- t, f x > f h]

-- Usa insertion sort
sortOnInsertion :: (Ord a, Ord b) => (a -> b) -> [a] -> [a]
sortOnInsertion _ [] = []
sortOnInsertion f (h:t) = insere f h (sortOnInsertion f t)

insere :: Ord b => (a -> b) -> a -> [a] -> [a]
insere _ x [] = [x]
insere f x (h:t)
    | f x > f h = h : insere f x t
    | otherwise = x : (h:t)

-- Usa merge sort
sortOnMerge :: (Ord a, Ord b) => (a -> b) -> [a] -> [a]
sortOnMerge _ [] = []
sortOnMerge _ [x] = [x]
sortOnMerge f l = let (x,y) = parteLista l in merge (sortOnMerge f x) (sortOnMerge f y)
    where parteLista [] = ([],[])
          parteLista l = let x = ((length l) `div` 2) in (take x l, drop x l)
        
merge :: Ord a => [a] -> [a] -> [a]
merge [] l = l
merge l [] = l
merge l@(h:t) l'@(h':t')
    | h < h' = h : merge t l'
    | otherwise = h' : merge l t'

-- Usa bubble sort
sortOnBubble :: (Ord a, Ord b) => (a -> b) -> [a] -> [a]
sortOnBubble f l = bubble l 0
    where bubble l i
            | i == length l = l
            | otherwise = bubble (iterator f l) (i + 1)

iterator :: (Ord a, Ord b) => (a -> b) -> [a] -> [a]
iterator f (h:x:t)
    | f h > f x = x : iterator f (h:t)
    | otherwise = h : iterator f (x:t)
iterator _ l = l

-- Exercício 2
type Polinomio = [Monomio]
type Monomio = (Float,Int)

-- a)
selgrau :: Int -> Polinomio -> Polinomio
selgrau grau l = filter (\elem -> grau == snd elem) l

-- b)
conta :: Int -> Polinomio -> Int
conta grau l = length $ filter (\elem -> grau == snd elem) l

-- c)
grau :: Polinomio -> Int
grau l = maximum $ map snd l

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
ordena :: Polinomio -> Polinomio
ordena l = sortBy (compare `on` snd) l

-- i)
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

-- j)
soma :: Polinomio -> Polinomio -> Polinomio
soma l l' = normaliza (l ++ l')

-- k)
produto :: Polinomio -> Polinomio -> Polinomio
produto (h:t) l = soma (mult h l) (produto t l)

-- l)
equiv :: Polinomio -> Polinomio -> Bool
equiv l l' = (ordena $ normaliza l) == (ordena $ normaliza l')

-- Exercício 3
type Mat a = [[a]]

-- a)
dimOK :: Mat a -> Bool
dimOK (h:t) = all (\elem -> length h == length elem) t

-- b)
dimMat :: Mat a -> (Int,Int)
dimMat l@(h:t) = (length l, length h)

-- c)
addMat :: Num a => Mat a -> Mat a -> Mat a
addMat [] [] = []
addMat (h:t) (h':t') = addLinha h h' : (addMat t t')
    where addLinha [] [] = []
          addLinha (h:t) (h':t') = (h + h') : addLinha t t'

-- Apenas uma linha
addMat' :: Num a => Mat a -> Mat a -> Mat a
addMat' = zipWith (zipWith (+))

-- d)
transpose :: Mat a -> Mat a
transpose [] = []
transpose ([]:_) = []
transpose l = (map head l) : (transpose $ map tail l)

-- e)
multMat :: Num a => Mat a -> Mat a -> Mat a
multMat [] l = l
multMat l [] = l
multMat l l' = map (multMat' [] l') l
    where multMat' l [] _ = l
          multMat' l _ [] = l
          multMat' [] (h:t) (h':t') = multMat' (map (h'*) h) t t'
          multMat' l (h:t) (h':t') = multMat' (zipWith (\x y -> x + y * h') l h) t t'
    
-- Usa a transpose
multMat' :: Num a => Mat a -> Mat a -> Mat a
multMat' [] l = l
multMat' l [] = l
multMat' l l' = [[sum $ zipWith (*) x y | x <- (transpose l'), y <- l]]

-- f)
zipWMat :: (a -> b -> c) -> Mat a -> Mat b -> Mat c
zipWMat _ [] _ = []
zipWMat _ _ [] = []
zipWMat f (h:t) (h':t') = zipWith f h h' : zipWMat f t t'

-- Apenas uma linha
zipWMat' :: (a -> b -> c) -> Mat a -> Mat b -> Mat c
zipWMat' = zipWith . zipWith

-- g)
triSup :: (Ord a, Num a) => Mat a -> Bool
triSup l = triSup' (abaixoDiagonal l 0)
    where abaixoDiagonal [] _ = []
          abaixoDiagonal (h:t) x = (take x h) : (abaixoDiagonal t (x + 1))

triSup':: (Ord a, Num a) => Mat a -> Bool
triSup' [] = True
triSup' (h:t)
    | all (== 0) h = triSup' t
    | otherwise = False

-- h)
rotateLeft :: Mat a -> Mat a
rotateLeft [] = []
rotateLeft ([]:_) = []
rotateLeft l = (map last l) : (rotateLeft $ map init l)

-- Usa a transpose
rotateLeft' :: Mat a -> Mat a
rotateLeft' [] = []
rotateLeft' l = reverse $ transpose l
