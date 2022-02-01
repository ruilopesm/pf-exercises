module Exame1920 where

import System.Random

-- Exercício 1

-- a)
inits :: [a] -> [[a]]
inits [] = [[]]
inits l = inits (init l) ++ [l]

-- b)
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (h:t) (h':t') = h == h' && isPrefixOf t t'

-- Exercício 2
data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show

-- a)
folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node a Empty Empty) = 1
folhas (Node a e d) = (folhas e) + (folhas d)

-- b)
path :: [Bool] -> BTree a -> [a]
path _ Empty = []
path (h:t) (Node a e d)
    | h = a : path t d
    | otherwise = a : path t e

-- Exercício 3
type Polinomio = [Coeficiente]
type Coeficiente = Float

-- a)
valor :: Polinomio -> Float -> Float
valor l x = valorAux l x 0
    where valorAux [] x 0 = 0
          valorAux (h:t) x pos
            | h == 0 = valorAux t x (pos + 1)
            | otherwise = h * (x ^ pos) + valorAux t x (pos + 1)

-- b)
deriv :: Polinomio -> Polinomio
deriv l = tail $ derivAux l 0
    where derivAux [] _ = []
          derivAux (h:t) pos = (pos * h) : derivAux t (pos + 1)

-- c)
soma :: Polinomio -> Polinomio -> Polinomio
soma = zipWith (+)

-- Exercício 4
type Mat a = [[a]]

-- a)
quebraLinha :: [Int] -> [a] -> [[a]]
quebraLinha [] _ = []
quebraLinha (h:t) m = take h m : quebraLinha t (drop h m)

-- b)
fragmenta :: [Int] -> [Int] -> Mat a -> [Mat a]
fragmenta [] _ _ = []
fragmenta (h:t) l m = quebraLinhas l (take h m) ++ fragmenta t l (drop h m)

quebraLinhas :: [Int] -> Mat a -> [Mat a]
quebraLinhas [] _ = []
quebraLinhas (h:t) m = map (take h) m : quebraLinhas t (map (drop h) m)

-- c)
geraMat :: (Int,Int) -> (Int,Int) -> IO (Mat Int)
geraMat (0,_) _ = return []
geraMat (x,y) (a,b) = do
    h <- geraLinha y (a,b)
    t <- geraMat (x - 1,y) (a,b)
    return (h:t)

geraLinha :: Int -> (Int,Int) -> IO [Int]
geraLinha 0 _ = return []
geraLinha x (a,b) = do
    h <- randomRIO (a,b)
    t <- geraLinha (x - 1) (a,b)
    return (h:t)
