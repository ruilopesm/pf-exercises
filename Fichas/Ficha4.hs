module Ficha4 where

import Prelude hiding (divMod)
import Data.Char
import Data.List

-- Exercício 1
digitAlpha :: String -> (String, String)
digitAlpha [] = ([],[])
digitAlpha (h:t)
    | isAlpha h = (h : x, y)
    | otherwise = (x,h : y)
    where (x,y) = digitAlpha t

-- Exercício 2
nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (h:t)
    | h < 0 = (1 + x,y,z)
    | h == 0 = (x,1 + y,z)
    | otherwise = (x,y,1 + z)
    where (x,y,z) = nzp t

-- Exercício 3
divMod :: Integral a => a -> a -> (a,a)
divMod x y
    | x < y = (0,x)
    | otherwise = (1 + a,b)
    where (a,b) = divMod (x - y) y

-- Exercício 4
fromDigits :: [Int] -> Int
fromDigits l = foldl (\acc elem -> elem + 10 * acc) 0 l

-- Exercício 5
maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit l = foldr (\elem acc -> elem + acc) 0 l

-- Exercício 6
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib x = fib' x 0 1 
    where fib' :: Int -> Int -> Int -> Int
          fib' x a b
            | x == 0 = a
            | otherwise = fib' (x - 1) b (a + b) 

-- Exercício 7
intToStr :: Integer -> String
intToStr x = intToStr' x []
    where intToStr' :: Integer -> String -> String
          intToStr' 0 l = l
          intToStr' x l = intToStr' (x `div` 10) ((show $ x `mod` 10) ++ l)

-- Exercício 8

-- a)
-- [6,12,18]
alineaA :: [Integer]
alineaA = [x | x <- [1..20], mod x 6 == 0]

-- b)
-- [6,12,18]
alineaB :: [Integer]
alineaB = [x | x <- [1..20], mod x 6 == 0]

-- c)
-- [(10,20),(11,19),(12,18),(13,17),(14,16),(15,15),(16,14),(17,13),(18,12),(19,11),(20,10)]
alineaC :: [(Integer,Integer)]
alineaC = undefined

-- d)
-- [1,1,4,4,9,9,16,16,25,25]
alineaD :: [Integer]
alineaD = undefined

-- Exercício 9

-- a)
alineaA' :: [Integer]
alineaA' = [2 ^ x | x <- [0..10]]

-- b)
alineaB' :: [(Integer,Integer)]
alineaB' = [(x,y) | x <- [1..5], y <- [1..5], x + y == 6]

-- c)
alineaC' :: [[Integer]]
alineaC' = [x | x <- drop 1 $ inits [1..5]]

-- d)
alineaD' :: [[Integer]]
alineaD' = [replicate x 1 | x <- [1..5]]

-- e)
alineaE' :: [Integer]
alineaE' = [factorial x | x <- [1..6]]
    where factorial :: Integer -> Integer
          factorial 0 = 1
          factorial x = x * factorial (x - 1)
