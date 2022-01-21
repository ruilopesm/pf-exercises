module Ficha1 where

import Data.Char (ord, chr)

-- Exercício 1

-- a)
perimetro :: Float -> Float
perimetro x = 2 * pi * x

-- b)
dist :: (Double, Double) -> (Double, Double) -> Double
dist (x,y) (x',y') = sqrt (distx + disty)
    where distx = (x - x') ^ 2
          disty = (y - y') ^ 2

-- c)
primUlt :: [a] -> (a,a)
primUlt l = (head l, last l)

-- d)
multiplo :: Int -> Int -> Bool
multiplo x y = mod x y == 0

-- e)
truncaImpar :: [a] -> [a]
truncaImpar l@(h:t)
    | mod (length l) 2 /= 0 = t
    | otherwise = l

-- f)
max2 :: Int -> Int -> Int
max2 x y
    | x > y = x
    | otherwise = y

-- g)
max3 :: Int -> Int -> Int -> Int
max3 x y z = max2 (max2 x y) z 

-- Exercício 2

-- a)
nRaizes :: Float -> Float -> Float -> Int
nRaizes a b c
    | delta > 0 = 2
    | delta == 0 = 1
    | otherwise = 0
    where delta = (b ^ 2) - (4 * a * c)

-- b)
raizes :: Float -> Float -> Float -> [Float]
raizes a b c
    | x > 0 = [r1, r2]
    | x == 0 = [r1]
    | otherwise = []
    where x = nRaizes a b c
          (r1, r2) = (((-b) + sqrt (delta)) / (2 * a), (((-b) - sqrt (delta)) / (2 * a))) 
          delta = (b ^ 2) - (4 * a * c)

-- Exercício 3
type Hora = (Int, Int)

-- a)
validaHora :: Hora -> Bool
validaHora (f,s) = (f >= 0 && f <= 23) && (s >= 0 && s <= 59)

-- b)
horaDepois :: Hora -> Hora -> Bool
horaDepois (f,s) (f',s') = f > f' || (f == f' && s > s')

-- c)
horaParaMinutos :: Hora -> Int
horaParaMinutos (f,s) = (f * 60) + s

-- d)
minutosParaHora :: Int -> Hora
minutosParaHora x = (div x 60, mod x 60)

-- e)
diferencaHoras :: Hora -> Hora -> Int
diferencaHoras (f,s) (f',s') = horaParaMinutos (abs $ f - f', abs $ s - s')

-- f)
adicionaMinutos :: Int -> Hora -> Hora
adicionaMinutos x h = minutosParaHora (x + (horaParaMinutos h))

-- Exercício 4
data NovaHora = H Int Int deriving (Show, Eq)

-- a)
validaNovaHora :: NovaHora -> Bool
validaNovaHora (H h m) = (h >= 0 && h <= 23) && (m >= 0 && m <= 59)

-- b)
novaHoraDepois :: NovaHora -> NovaHora -> Bool
novaHoraDepois (H h m) (H h' m') = h > h' || (h == h' && m > m')

-- c)
novaHoraParaMinutos :: NovaHora -> Int
novaHoraParaMinutos (H h m) = (h * 60) + m

-- d)
minutosParaNovaHora :: Int -> NovaHora
minutosParaNovaHora x = (H (div x 60) (mod x 60))

-- e)
diferencaNovasHoras :: NovaHora -> NovaHora -> Int
diferencaNovasHoras (H h m) (H h' m') = novaHoraParaMinutos (H (abs $ h - h') (abs $ m - m'))

-- f)
adicionaMinutos' :: Int -> NovaHora -> NovaHora
adicionaMinutos' x h = minutosParaNovaHora (x + (novaHoraParaMinutos h)) 

-- Exercício 5
data Semaforo = Verde | Amarelo | Vermelho deriving (Show, Eq)

-- a)
next :: Semaforo -> Semaforo
next Verde = Amarelo
next Amarelo = Vermelho
next Vermelho = Verde

-- b)
stop :: Semaforo -> Bool
stop Vermelho = True
stop _ = False

-- c)
safe :: Semaforo -> Semaforo -> Bool
safe Vermelho _ = True
safe _ Vermelho = True
safe _ _ = False

-- Exercício 6
data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show, Eq)

-- a)
posx :: Ponto -> Double
posx (Cartesiano x _) = x
posx (Polar dist angle) 
    | angle == pi / 2 = 0
    | otherwise = dist * cos angle

-- b)
posy :: Ponto -> Double
posy (Cartesiano _ y) = y
posy (Polar dist angle)
    | angle == pi / 2 = 0
    | otherwise = dist * sin angle

-- c)
raio :: Ponto -> Double
raio (Cartesiano x y) = dist (x,y) (0,0)
raio (Polar dist _) = dist

-- d)
angulo :: Ponto -> Double
angulo (Cartesiano x y)
    | x < 0 && y == 0 = pi
    | x < 0 = pi + atan (y / x)
    | otherwise = atan (y / x)
angulo (Polar _ angle) = angle

-- e)
distEntrePontos :: Ponto -> Ponto -> Double
distEntrePontos p p' = dist (posx p, posy p) (posx p', posy p')

-- Exercício 7
data Figura = Circulo Ponto Double
    | Rectangulo Ponto Ponto
    | Triangulo Ponto Ponto Ponto
    deriving (Show, Eq)

-- a)
poligono :: Figura -> Bool
poligono (Circulo p r) = False
poligono (Rectangulo p p') = (posx p /= posx p') && (posy p /= posy p')
poligono (Triangulo p1 p2 p3) = (posy p2 - posy p1) / (posx p2 - posx p1) /= (posy p3 - posy p2) / (posx p3 - posx p2)

-- b)
vertices :: Figura -> [Ponto]
vertices (Circulo _ _) = []
vertices rectang@(Rectangulo p p')
    | poligono rectang = [p, Cartesiano (posx p) (posy p'), p', Cartesiano (posx p') (posy p)]
    | otherwise = []
vertices triang@(Triangulo p1 p2 p3)
    | poligono triang = [p1, p2, p3]
    | otherwise = []

-- c)
area :: Figura -> Double
area (Triangulo p1 p2 p3) =
    let a = distEntrePontos p1 p2
        b = distEntrePontos p2 p3
        c = distEntrePontos p3 p1
        s = (a + b + c) / 2 -- semi-perimetro
    in sqrt (s * (s - a) * (s - b) * (s - c)) -- formula de Heron
area (Circulo _ r) = pi * (r ^ 2)
area (Rectangulo p p') = (abs $ posx p - posx p') * (abs $ posy p - posy p')

-- d)
perimetroFigura :: Figura -> Double
perimetroFigura (Circulo _ r) = 2 * pi * r
perimetroFigura (Rectangulo p p') = 2 * (abs $ posx p' - posx p) + 2 * (abs $ (posy p' - posy p))
perimetroFigura (Triangulo p1 p2 p3) = distEntrePontos p1 p2 + distEntrePontos p2 p3 + distEntrePontos p1 p3

-- Exercício 8

-- a)
isLower :: Char -> Bool
isLower c = c >= 'a' && c <= 'z'

-- b)
isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

-- c)
isAlpha :: Char -> Bool
isAlpha c = isLower c || isUpper c
    where isUpper :: Char -> Bool
          isUpper c = c >= 'A' && c <= 'Z'

-- d)
toUpper :: Char -> Char
toUpper c
    | isLower c = chr $ (ord c - 32)
    | otherwise = c

-- e)
intToDigit :: Int -> Char
intToDigit x = chr (x + 48)

-- f)
digitToInt :: Char -> Int
digitToInt c = ord c - 48
