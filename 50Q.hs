module FiftyQ where 

import Prelude hiding (enumFromTo, enumFromThenTo, (++), (!!), reverse, take, drop, zip, replicate, concat, unwords, unlines, lookup)

-- 1.
enumFromTo :: Int -> Int -> [Int]
enumFromTo start end
    | start > end = []
    | otherwise = start : enumFromTo (start + 1) end

-- 2.
enumFromThenTo :: Int -> Int -> Int -> [Int]
enumFromThenTo start space end
    | start > end && space > start || start < end && space < start || start == space && start > end = []
    | otherwise = start : enumFromThenTo space (2 * start - space) end

-- 3.
(++) :: [a] -> [a] -> [a]
(++) [] [] = []
(++) [] l = l
(++) l [] = l
(++) (h:t) l = h : (++) t l

-- 4.
(!!) :: [a] -> Int -> a 
(!!) (h:t) 0 = h
(!!) (h:t) x = (!!) t (x - 1)

-- 5.
reverse :: [a] -> [a]
reverse [] = []
reverse (h:t) = reverse t ++ [h]

-- 6.
take :: Int -> [a] -> [a]
take _ [] = []
take x (h:t)
    | x <= 0 = []
    | otherwise = h : take (x - 1) t

-- 7.
drop :: Int -> [a] -> [a]
drop _ [] = []
drop x l@(h:t)
    | x <= 0 = l
    | otherwise = drop (x - 1) t

-- 8.
zip :: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip _ [] = []
zip (h:t) (h':t') = (h,h') : zip t t'

-- 9.
replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate x elem = elem : replicate (x - 1) elem

-- 10.
intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse elem (h:t) = h : elem : intersperse elem t

-- 11.
group :: Eq a => [a] -> [[a]]
group [] = [[]]
group [x] = [[x]]
group (h:t)
    | h `elem` (head r) = (h: (head r)) : tail r
    | otherwise = [h] : r
    where r = group t 

-- Com funções de ordem superior
group' :: Eq a => [a] -> [[a]]
group' [] = [[]]
group' (h:t) = (h : takeWhile (== h) t) : group' (dropWhile (== h) t)

-- 12.
concat :: [[a]] -> [a]
concat [] = []
concat (h:t) = h ++ concat t

-- Com funções de ordem superior
concat' :: [[a]] -> [a]
concat' l = foldr (++) [] l

-- 13.
inits :: [a] -> [[a]]
inits [] = [[]]
inits l = inits (init l) ++ [l]

-- 14.
tails :: [a] -> [[a]]
tails [] = [[]]
tails l = l : (tails (tail l))

-- 15.
heads :: [[a]] -> [a]
heads [] = []
heads ([]:t) = heads t
heads (h:t) = head h : heads t

-- 16.
total :: [[a]] -> Int
total [] = 0
total (h:t) = totalAux h 0 + total t
    where totalAux [] x = x
          totalAux (h:t) x = totalAux t (x + 1)

-- Com funções de ordem superior
total' :: [[a]] -> Int
total' l = length $ foldr (++) [] l

-- 17.
fun :: [(a,b,c)] -> [(a,c)]
fun [] = []
fun ((x,y,z):t) = (x,z) : fun t

-- Com funções de ordem superior
fun' :: [(a,b,c)] -> [(a,c)]
fun' l = map (\(x,y,z) -> (x,z)) l

-- 18.
cola :: [(String,b,c)] -> String
cola [] = ""
cola ((s,_,_):t) = s ++ cola t

-- 19.
idade :: Int -> Int -> [(String,Int)] -> [String]
idade _ _ [] = []
idade atual x ((s,i):t)
    | atual - x >= i = s : idade atual x t
    | otherwise = idade atual x t

-- 20.
powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom n m = powerEnumFromAux n m 0
    where powerEnumFromAux n m x
            | m == x = []
            | otherwise = n ^ x : powerEnumFromAux n m (x + 1)

-- 21.
isPrime :: Int -> Bool
isPrime n = isPrimeAux n 2
    where isPrimeAux n m
            | m * m > n = True
            | mod n m == 0 = False
            | otherwise = isPrimeAux n (m + 1)

-- 22.
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = False
isPrefixOf _  [] = True
isPrefixOf (h:t) (h':t') = h == h' && isPrefixOf t t'

-- 23.
isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf [] _ = True
isSuffixOf _ [] = False
isSuffixOf l l'@(_:t) = l == l' || isSuffixOf l t

-- 24.
isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf _ [] = False
isSubsequenceOf [] _ = True
isSubsequenceOf l@(h:t) (h':t') = h == h' && isSubsequenceOf t t' || isSubsequenceOf l t

-- 25.
elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices _ [] = []
elemIndices x l = elemIndicesAux x l 0
    where elemIndicesAux _ [] _ = []
          elemIndicesAux x (h:t) i 
            | x == h = i : elemIndicesAux x t (i + 1)
            | otherwise = elemIndicesAux x t (i + 1)

-- 26.
nub :: Eq a => [a] -> [a]
nub [] = []
nub (h:t)
    | h `elem` t = nub t
    | otherwise = h : nub t

-- 27.
delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete x (h:t)
    | x == h = t
    | otherwise = h : delete x t

-- 28.
(\\) :: Eq a => [a] -> [a] -> [a]
(\\) [] _ = []
(\\) l [] = l
(\\) l (h:t) = (\\) (delete h l) t 

-- 29.
union :: Eq a => [a] -> [a] -> [a]
union [] _ = []
union l [] = l
union l (h:t)
    | h `elem` l = union l t
    | otherwise = union (l ++ [h]) t

-- 30.
intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []
intersect (h:t) l
    | h `elem` l = h : intersect t l
    | otherwise = intersect t l

-- 31.
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (h:t)
    | x > h = h : insert x t
    | otherwise = x : h : t

-- 32.
unwords :: [String] -> String
unwords [] = ""
unwords (h:[]) = h
unwords (h:t) = h ++ " " ++ unwords t

-- 33.
unlines :: [String] -> String
unlines [] = ""
unlines (h:t) = h ++ "\n" ++ unlines t

-- 34.
pMaior :: Ord a => [a] -> Int
pMaior [x] = 0
pMaior l = let m = maximum l in (pMaiorAux l m)
    where pMaiorAux (h:t) m
            | h == m = 0
            | otherwise = 1 + pMaiorAux t m

-- 35.
lookup :: Eq a => a -> [(a,b)] -> Maybe b
lookup _ [] = Nothing
lookup x ((a,b):t)
    | x == a = Just b
    | otherwise = lookup x t

-- 36.
preCrescente :: Ord a => [a] -> [a]
preCrescente [] = []
preCrescente [x] = [x]
preCrescente (h:x:t)
    | x >= h = h : preCrescente (x:t)
    | otherwise = [h]

-- 37.
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = insert h (iSort t)

-- 38.
menor :: String -> String -> Bool
menor  "" _ = True
menor _ "" = False
menor (h:t) (h':t')
    | h < h' = True
    | h == h' = menor t t'
    | otherwise = False

-- 39.
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet _ [] = False
elemMSet x ((f,s):t)
    | x == f = True
    | otherwise = elemMSet x t

-- 40.
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((f,s):t) = replicate s f ++ converteMSet t

-- 41.
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet x [] = [(x,1)]
insereMSet x ((f,s):t)
    | x == f = (f,s + 1) : t
    | otherwise = (f,s) : insereMSet x t

-- 42.
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet _ [] = []
removeMSet x ((f,s):t)
    | x == f && (s - 1) > 0 = (f,s - 1) : t
    | x == f = t
    | otherwise = (f,s) : removeMSet x t

-- 43.
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet l = insereMSet (last l) (constroiMSet (init l))

-- 44.
partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers [] = ([],[])
partitionEithers (h:t) =
    case h of
        Left a -> (a:x,y)
        Right a -> (x,a:y)
    where (x,y) = partitionEithers t

-- 45.
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (h:t) =
    case h of
        Just a -> a : catMaybes t
        Nothing -> catMaybes t

-- 46.
data Movimento = Norte | Sul | Este | Oeste deriving Show

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x,y) (x',y')
    | x < x' = Este : caminho (x + 1, y) (x',y')
    | x > x' = Oeste : caminho (x - 1,y) (x',y')
    | y < y' = Norte : caminho (x,y + 1) (x',y')
    | y > y' = Sul : caminho (x,y - 1) (x',y')
    | otherwise = []

-- 47.
hasLoops :: (Int,Int) -> [Movimento] -> Bool
hasLoops _ [] = False
hasLoops (x,y) l = hasLoopsAux (x,y) (x,y) l
    where hasLoopsAux _ _ [] = False 
          hasLoopsAux (x,y) (x',y') (h:t)
            | movimenta (x,y) h == (x',y') = True
            | otherwise = hasLoopsAux (movimenta (x,y) h) (x',y') t

movimenta :: (Int,Int) -> Movimento -> (Int,Int)
movimenta (x,y) mov =
    case mov of 
        Norte -> (x,y + 1)
        Sul -> (x,y - 1)
        Este -> (x + 1,y)
        Oeste -> (x - 1,y)

-- 48.
type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto

contaQuadrados :: [Rectangulo] -> Int
contaQuadrados [] = 0
contaQuadrados ((Rect (x,y) (x',y')):t)
    | abs (x - x') == abs (y - y') = 1 + contaQuadrados t
    | otherwise = contaQuadrados t

-- 49.
areaTotal :: [Rectangulo] -> Float
areaTotal [] = 0.0
areaTotal ((Rect (x,y) (x',y')):t) = (abs (x - x')) * (abs (y - y')) + areaTotal t

-- 50.
data Equipamento = Bom | Razoavel | Avariado deriving Show

naoReparar :: [Equipamento] -> Int
naoReparar [] = 0
naoReparar (h:t) =
    case h of 
        Avariado -> naoReparar t
        _ -> 1 + naoReparar t
