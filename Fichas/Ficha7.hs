module Ficha7 where

-- Exercício 1
data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt

-- a)
calcula :: ExpInt -> Int
calcula exp =
    case exp of
        Const x -> x
        Simetrico x -> (-1) * (calcula x)
        Mais x y -> (calcula x) + (calcula y)
        Menos x y -> (calcula x) - (calcula y)
        Mult x y -> (calcula x) * (calcula y)

-- b)
infixa :: ExpInt -> String
infixa exp =
    case exp of
        Const x -> show x
        Simetrico x -> "(-" ++ infixa x ++ ")"
        Mais x y -> "(" ++ infixa x ++ " + " ++ infixa y ++ ")"
        Menos x y -> "(" ++ infixa x ++ " - " ++ infixa y ++ ")"
        Mult x y -> "(" ++ infixa x ++ " * " ++ infixa y ++ ")"

-- c)
posfixa :: ExpInt -> String
posfixa exp =
    case exp of
        Const x -> show x ++ " "
        Simetrico x -> "-" ++ posfixa x
        Mais x y -> posfixa x ++ posfixa y ++ "+ "
        Menos x y -> posfixa x ++ posfixa y ++ "- "
        Mult x y -> posfixa x ++ posfixa y ++ "* "

-- Exercício 2
data RTree a = R a [RTree a]

-- a)
soma :: Num a => RTree a -> a
soma (R a []) = a
soma (R a l) = a + (sum $ map soma l)

-- b)
altura :: RTree a -> Int
altura (R a []) = 1
altura (R a l) = 1 + (maximum $ map altura l)

-- c)
prune :: Int -> RTree a -> RTree a
prune 0 (R a _) = R a []
prune x (R a l) = R a (map (prune (x - 1)) l)

-- d)
mirror :: RTree a -> RTree a
mirror (R a l) = R a (map mirror $ reverse l)

-- e)
postorder :: RTree a -> [a]
postorder (R a []) = [a]
postorder (R a l) = (concat $ map postorder l) ++ [a]

-- Exercício 3
data BTree a = Empty | Node a (BTree a) (BTree a)
data LTree a = Tip a | Fork (LTree a) (LTree a)

-- a)
ltSum :: Num a => LTree a -> a
ltSum (Tip a) = a
ltSum (Fork e d) = ltSum e + ltSum d

-- b)
listaLT :: LTree a -> [a]
listaLT (Tip a) = [a]
listaLT (Fork e d) = listaLT e ++ listaLT d

-- c)
ltHeight :: LTree a -> Int
ltHeight (Tip a) = 0
ltHeight (Fork e d) = 1 + max (ltHeight e) (ltHeight d)

-- Exercício 4
data FTree a b = Leaf b | No a (FTree a b) (FTree a b)

-- a)
splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf b) = (Empty, Tip b)
splitFTree (No a e d) = (Node a x x', Fork y y')
    where (x,y) = splitFTree e
          (x',y') = splitFTree d

-- b)
joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees Empty (Tip x) = Just $ Leaf x
joinTrees (Node a e d) (Fork x y) = Just $ No a x' y'
    where Just x' = joinTrees e x
          Just y' = joinTrees d y
joinTrees _ _ = Nothing
