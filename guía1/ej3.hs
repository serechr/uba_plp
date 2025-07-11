-- Ejercicio 3

-- Redefinir usando foldr las funciones sum, elem, (++), filter y map

-- foldr' :: (a -> b -> b) -> b -> t a -> b

sum' :: [Integer] -> Integer
sum' = foldr (+) 0

-- elem' :: Eq a => a -> [a] -> Bool

-- con recursion explicita
elem1 _ [] = False
elem1 n (x:xs) = if n == x then True else elem1 n xs

-- version de Joni
elem2 n = foldr ((||) . (== n)) False

-- con foldr y lambda
elem3 n = foldr (\x rec -> x == n || rec) False

-- (++) [a] -> [a] -> [a]

-- con recursion explicita
concat1 :: [a] -> [a] -> [a]
concat1 xs [] = xs
concat1 [] ys = ys
concat1 (x:xs) ys = x : concat1 xs ys

-- esta rara, mi acumulador va a ser ys y voy a ir concatenandole los elems de xs "por delante"
concat2 xs ys = foldr (\x rec -> x : rec) ys xs

-- con recursión explícita
filtrar1 :: (a -> Bool) -> [a] -> [a]
filtrar1 _ [] = []
filtrar1 p (x:xs) = if p x then x : rec else rec
    where rec = filtrar1 p xs

-- con foldr y lambda
filtrar2 p = foldr (\x rec -> if p x then x : rec else rec) []

-- con recursión explícita
mapeo1 :: (a -> b) -> [a] -> [b]
mapeo1 _ [] = []
mapeo1 f (x:xs) = f x : mapeo1 f xs

-- con foldr y lambda
mapeo2 f = foldr (\x rec -> f x : rec) []

-- foldr1: it takes the last two items of the list and applies the function, 
-- then it takes the third item from the end and the result, and so on.
mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun p = foldr1 (\x y -> if p x y then x else y)

sumasParciales :: Num a => [a] -> [a]
sumasParciales = foldl (\ls x -> if null ls then [x] else [x + last ls]) []

-- sumaAlt :: [a] -> a
sumaAlt xs = foldr (\x acc -> (-) x acc) 0 xs

sumaAlt' :: Num a => [a] -> a
sumaAlt' = foldl (flip (-)) 0