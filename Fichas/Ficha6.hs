module Ficha6 where

import GHC.Float

-- Exercício 1
data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show

-- a)
altura :: BTree a -> Int
altura Empty = 0
altura (Node a e d) = 1 + max (altura e) (altura d)

-- b)
contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node a Empty Empty) = 1
contaNodos (Node a e d) = 1 + (contaNodos e) + (contaNodos d)

-- c)
folhas :: BTree a -> Int
folhas (Node a Empty Empty) = 1
folhas (Node a e d) = (folhas e) + (folhas d)

-- d)
prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune 0 _ = Empty
prune x (Node a e d) = Node a (prune (x - 1) e) (prune (x - 1) d)

-- e)
path :: [Bool] -> BTree a -> [a]
path _ Empty = []
path [] (Node a e d) = [a]
path (h:t) (Node a e d) = a : path t (if h then d else e)

-- f)
mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node a e d) = Node a (mirror d) (mirror e)

-- g)
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT _ Empty Empty = Empty
zipWithBT f (Node a e d) (Node a' e' d') = Node (f a a') (zipWithBT f e e') (zipWithBT f d d')

-- h)
unzipBT :: BTree (a,b,c) -> (BTree a, BTree b, BTree c)
unzipBT Empty = (Empty,Empty,Empty)
unzipBT (Node (a,b,c) e d) = (Node a x x', Node b y y', Node c z z')
    where (x,y,z) = unzipBT e
          (x',y',z') = unzipBT d

-- Exercício 2

-- a)
minimo :: Ord a => BTree a -> a
minimo (Node a Empty _) = a
minimo (Node a e d) = minimo e

-- b)
semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node _ Empty _) = Empty
semMinimo (Node a e d) = Node a (semMinimo e) d

-- c)
minSmin :: Ord a => BTree a -> (a, BTree a)
minSmin (Node a Empty _) = (a, Empty)
minSmin (Node a e d) = (x, Node a y d)
    where (x,y) = minSmin e

-- d)
remove :: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty
remove x tree@(Node a e d)
    | x < a = Node a (remove x e) d
    | x > a = Node a e (remove x d)
    | otherwise = removeAux x tree
    where removeAux x (Node a e d) =
            case e of
                Empty -> d
                _ -> case d of
                        Empty -> e
                        _ -> Node x e y
          (x,y) = minSmin d

-- Exercício 3
type Aluno = (Numero, Nome, Regime, Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int | Rep | Faltou deriving Show
type Turma = BTree Aluno

-- a)
inscNum :: Numero -> Turma -> Bool
inscNum _ Empty = False
inscNum x (Node (num,_,_,_) e d)
    | x == num = True
    | otherwise = inscNum x (if x < num then e else d)

-- b)
inscNome :: Nome -> Turma -> Bool
inscNome _ Empty = False
inscNome x (Node (_,nome,_,_) e d)
    | x == nome = True
    | otherwise = inscNome x d || inscNome x e

-- c)
trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (num,nome,reg,_) e d) =
    case reg of
        TE -> (num,nome) : (trabEst e ++ trabEst d)
        _ -> [] ++ trabEst e ++ trabEst d

-- d)
nota :: Numero -> Turma -> Maybe Classificacao
nota _ Empty = Nothing
nota x (Node (num,_,_,cla) e d)
    | x == num = Just cla
    | otherwise = nota x (if x < num then e else d)

-- e)
percFaltas :: Turma -> Float
percFaltas tree = ((percFaltasAux tree) * 100) / (fromIntegral $ total)
    where total = contaNodos tree
          percFaltasAux Empty = 0
          percFaltasAux (Node (_,_,_,cla) e d) =
              case cla of
                  Faltou -> 1 + percFaltasAux e + percFaltasAux d
                  _ -> percFaltasAux e + percFaltasAux d

-- f)
mediaAprov :: Turma -> Float
mediaAprov tree = (int2Float (sum $ mediaAprovAux tree)) / (int2Float $ total)
    where total = contaNodos tree
          mediaAprovAux Empty = []
          mediaAprovAux (Node (_,_,_,cla) e d) =
              case cla of
                  Aprov x -> [x] ++ (mediaAprovAux e) ++ (mediaAprovAux d)
                  _ -> (mediaAprovAux e) ++ (mediaAprovAux d)

-- g)
aprovAv :: Turma -> Float
aprovAv tree = aprovados / avaliados
    where (aprovados, avaliados) = aprovAvAux tree
          aprovAvAux Empty = (0,0)
          aprovAvAux (Node (_,_,_,cla) e d) =
              case cla of
                  Aprov _ -> (a + 1,b)
                  Rep -> (a,b + 1)
                  _ -> (a,b)
              where (x,y) = aprovAvAux e
                    (x',y') = aprovAvAux d
                    (a,b) = (x + x',y + y')
