module Ficha3 where

import Ficha1
import Data.Maybe
import Data.List
import Data.Function

-- Exercício 1
{- data Hora = H Int Int
            deriving Show
-} -- Este data type já está definido na Ficha 1

type Etapa = (Ficha1.NovaHora, Ficha1.NovaHora)
type Viagem = [Etapa]

-- a)
validaEtapa :: Etapa -> Bool
validaEtapa (h,h') = validaNovaHora h && validaNovaHora h' && h' `novaHoraDepois` h

-- b)
validaViagem :: Viagem -> Bool
validaViagem [] = True
validaViagem [e] = validaEtapa e
validaViagem (h:x:t) = validaEtapa h && validaEtapa x && validaViagem (x:t)

-- c)
partidaChegada :: Viagem -> (Ficha1.NovaHora, Ficha1.NovaHora)
partidaChegada [e@(h,h')] = e
partidaChegada ((h,h'):(h3,h4):t) = partidaChegada ((h,h4):t)

-- d)
tempoViagem :: Viagem -> Int
tempoViagem [] = 0
tempoViagem ((h,h'):t) = diferencaNovasHoras h h' + tempoViagem t

-- e)
tempoEspera :: Viagem -> Int
tempoEspera [] = 0
tempoEspera ((h,h'):s@(h3,h4):t) = diferencaNovasHoras h' h3 + tempoEspera (s:t)

-- f)
tempoTotal :: Viagem -> Int
tempoTotal v = tempoViagem v + tempoEspera v

-- Exercício 2
type Poligonal = [Ficha1.Ponto]

-- a)
comprimento :: Poligonal -> Double
comprimento [] = 0
comprimento [x] = 0
comprimento (h:x:t) = distEntrePontos h x + comprimento t

-- b)
eFechada :: Poligonal -> Bool
eFechada l@(h:t) = length l >= 3 && posx h == posx (last l) && posy h == posy (last l)

-- c)
triangula :: Poligonal -> [Ficha1.Figura]
triangula [p1,p2,p3] = [Triangulo p1 p2 p3]
triangula (p1:p2:p3:t) = Triangulo p1 p2 p3 : triangula (p1:p3:t)

-- d)
areaPoligonal :: Poligonal -> Double
areaPoligonal l = sum $ map area (triangula l)

-- e)
mover :: Poligonal -> Ponto -> Poligonal
mover l p = p : l

-- f)
zoom :: Double -> Poligonal -> Poligonal
zoom factor [p, Cartesiano x y] = [p, Cartesiano (factor * x) (factor * y)]
zoom factor (h:(Cartesiano x y):t) = h : zoom factor (p':t)
    where p' = Cartesiano (factor * x) (factor * y)
zoom _ p = p

-- Exercício 3
data Contacto = Casa Integer
    | Trab Integer
    | Tlm Integer
    | Email String
    deriving Show

type Nome = String
type Agenda = [(Nome, [Contacto])]

-- a)
acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail nome s a = a ++ [(nome, [Email s])]

-- b)
verEmails :: Nome -> Agenda -> Maybe [String]
verEmails _ [] = Nothing
verEmails nome ((n,l):t)
    | nome == n = Just $ map (\(Email s) -> s) l
    | otherwise = verEmails nome t

-- c)
consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs (h:t) =
    case h of
        Casa x -> x : consTelefs t
        Trab x -> x : consTelefs t
        Tlm x -> x : consTelefs t
        _ -> consTelefs t

-- d)
casa :: Nome -> Agenda -> Maybe Integer
casa nome a
    | isJust $ encontraPorNome nome a = casa' (fromJust $ encontraPorNome nome a)
    | otherwise = Nothing
    where encontraPorNome :: Nome -> Agenda -> Maybe [Contacto]
          encontraPorNome _ [] = Nothing
          encontraPorNome nome ((n,l):t)
            | nome == n = Just l
            | otherwise = encontraPorNome nome t

casa' :: [Contacto] -> Maybe Integer
casa' [] = Nothing
casa' (h:t) =
    case h of
        Casa x -> Just x
        _ -> casa' t

-- Exercício 4
type Dia = Int 
type Mes = Int
type Ano = Int
type NovoNome = String

data Data = D Dia Mes Ano deriving Show

type TabDN = [(Nome, Data)]

-- a)
procura :: Nome -> TabDN -> Maybe Data
procura _ [] = Nothing
procura nome ((n,d):t)
    | nome == n = Just d
    | otherwise = procura nome t

-- b)
idade :: Data -> Nome -> TabDN -> Maybe Int
idade _ _ [] = Nothing
idade dt@(D d m a) nome ((n,D d' m' a'):t)
    | nome == n && (m > m' || m == m' && d > d') = Just (a - a')
    | nome == n = Just $ (a - a') - 1
    | otherwise = idade dt nome t 

-- c)
anterior :: Data -> Data -> Bool
anterior (D d m a) (D d' m' a')
    | a < a' = True
    | a == a' && m < m' = True
    | a == a' && m == m' && d < d' = True
    | otherwise = False

-- d)
ordena :: TabDN -> TabDN
ordena [] = []
ordena (h@(_,d):t) = menores ++ [h] ++ maiores
    where menores = ordena [x | x <- t, not $ anterior d (snd x)]
          maiores = ordena [x | x <- t, anterior d (snd x)]

-- e)
porIdade :: Data -> TabDN -> [(Nome,Int)]
porIdade _ [] = []
porIdade dt tab@((n,_):t) = (n,idade') : (porIdade dt t)
    where tabOrdenada = ordena tab
          idade' = fromJust $ idade dt n tabOrdenada

-- Exercício 4
data Movimento = Credito Float | Debito Float deriving Show

data NovaData = D' Int Int Int Int deriving Show

data Extracto = Ext Float [(Data, String, Movimento)] deriving Show

-- a)
extValor :: Extracto -> Float -> [Movimento]
extValor (Ext x ((_,_,mov):t)) y =
    case mov of
        Credito val
            | val >= y -> mov : extValor (Ext x t) y
            | otherwise -> extValor (Ext x t) y
        Debito val
            | val >= y -> mov : extValor (Ext x t) y
            | otherwise -> extValor (Ext x t) y

-- b)
filtro :: Extracto -> [String] -> [(Data,Movimento)]
filtro (Ext x ((dt,s,mov):t)) l
    | s `elem` l = (dt,mov) : filtro (Ext x t) l
    | otherwise = filtro (Ext x t) l

-- c)
creDeb :: Extracto -> (Float,Float)
creDeb (Ext x ((_,_,mov):t)) =
    case mov of
        Credito _ -> (1 + r1, r2)
        Debito _ -> (r1, 1 + r2)
    where (r1,r2) = creDeb (Ext x t)

-- d)
saldo :: Extracto -> Float
saldo (Ext x []) = x
saldo (Ext x l) = foldr (\(_,_,mov) acc ->
    case mov of
        Credito val -> acc + val
        Debito val -> acc - val) x l
