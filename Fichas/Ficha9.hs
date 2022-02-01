module Ficha9 where

import System.Random
import Data.List
import Data.Maybe

-- Exercício 1

-- a)
bingo :: IO ()
bingo = bingoAux []

bingoAux :: [Int] -> IO ()
bingoAux l
    | length l == 90 = return ()
    | otherwise = do geraNum l

geraNum :: [Int] -> IO ()
geraNum l = do 
    num <- randomRIO (1,90)
    if num `elem` l then bingoAux l else do
        getChar
        print num
        bingoAux (num:l)

-- b)
mastermind :: IO ()
mastermind = do
    x <- randomRIO (0,9)
    y <- randomRIO (0,9)
    z <- randomRIO (0,9)
    w <- randomRIO (0,9)
    let l = [x,y,z,w] in mastermindAux l
    
mastermindAux :: [Int] -> IO ()
mastermindAux l = do
    input <- getLine
    let inputLista = map read (words input) :: [Int]
    let total = acertou inputLista l
    if total /= 4 then do
        print $ "Digitos na posicao correta: " ++ show total
        print $ "Digitos na posicao errada: " ++ show (4 - total)
        mastermindAux l
    else print "Acabou"

acertou :: [Int] -> [Int] -> Int
acertou [] [] = 0
acertou (h:t) (h':t')
    | h == h' = 1 + acertou t t'
    | otherwise = acertou t t'

-- Exercício 2
data Aposta = Ap [Int] (Int,Int)

-- a)
valida :: Aposta -> Bool
valida (Ap l (e,e')) = validaIntervalo l [1..50] && validaIntervalo [e,e'] [1..9] && (length l == 5) && ((length $ nub l) == length l) && (e /= e')
    where validaIntervalo [] _ = True
          validaIntervalo (h:t) intervalo
            | h `elem` intervalo = validaIntervalo t intervalo
            | otherwise = False

-- b)
comuns :: Aposta -> Aposta -> (Int,Int)
comuns (Ap l (x,y)) (Ap l' (x',y')) = (comunsAux l l', comunsAux [x,y] [x',y']) 
    where comunsAux [] _ = 0 
          comunsAux (h:t) l
            | h `elem` l = 1 + comunsAux t l
            | otherwise = comunsAux t l 

-- c)
-- i.
instance Eq Aposta where
    (==) x y = comuns x y == (5,2)

-- ii.
premio :: Aposta -> Aposta -> Maybe Int
premio x y =
    case (comuns x y) of
        (5,n) -> Just (3 - n)
        (4,n) -> Just (6 - n)
        (3,n) -> Just (10 - n - (if n == 2 then 1 else 0))
        (2,2) -> Just 8 
        (1,2) -> Just 11
        (2,n) -> Just (13 - n)
        _ -> Nothing

-- d)
-- i.
leAposta :: IO Aposta
leAposta = do
    putStr $ "Introduza os numeros: "
    nums <- getLine
    let listaNums = map read (words nums) :: [Int]
    putStr $ "Introduza as estrelas: "
    estrelas <- getLine
    let parEstrelas = map read (words estrelas) :: [Int]
    let parEstrelas' = (parEstrelas !! 0, parEstrelas !! 1)
    let aposta = (Ap listaNums parEstrelas')
    if valida aposta then return aposta else do
        print $ "Aposta invalida"
        leAposta 
-- ii.
joga :: Aposta -> IO ()
joga chave = do
    aposta <- leAposta
    let flag = premio chave aposta
    if isJust $ flag then print $ fromJust $ premio chave aposta else print $ "Nao ganhou nada"

-- e)
geraChave :: IO Aposta
geraChave = do
    n1 <- randomRIO (1,50)
    n2 <- randomRIO (1,50)
    n3 <- randomRIO (1,50)
    n4 <- randomRIO (1,50)
    n5 <- randomRIO (1,50)
    e1 <- randomRIO (1,9)
    e2 <- randomRIO (1,9)
    let l = [n1,n2,n3,n4,n5]
    if (length $ nub l) == (length l) && (e1 /= e2) then return $ Ap l (e1,e2) else geraChave

-- f)
main :: IO ()
main = do
    ch <- geraChave
    ciclo ch

ciclo :: Aposta -> IO ()
ciclo chave = do
    opcao <- menu
    case opcao of
        "1" -> do
            joga chave
            ciclo chave
        "2" -> do
            print $ "Nova chave gerada"
            main
        "0" -> return ()

        _ -> ciclo chave

menu :: IO String
menu = do 
    let menutxt = unlines ["", "Apostar ........... 1", "Gerar nova chave .. 2", "", "Sair .............. 0"]
    putStrLn menutxt
    putStr "Opcao: "
    c <- getLine
    return c
