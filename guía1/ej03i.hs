-- Ejercicio 3

{- 
    Recordemos el tipo y definición de foldr:
        foldr :: (a -> b -> b) -> b -> t a -> b
        foldr f z []     = z                  -- Caso base
        foldr f z (x:xs) = f x (foldr f z xs) -- Caso recursivo
    
    Redefinir usando foldr las funciones sum, elem, (++), filter y map

    Hagamos siempre una versión usando recursión explícita y despues sí la de foldr como para ir agarrandole la mano al tema
-}

-- sum

sumRecExpl :: [Integer] -> Integer
sumRecExpl []     = 0
sumRecExpl (x:xs) = x + sumRecExpl xs

-- f sería (+) y z sería 0, despues de la recursión se encarga el fold :)

sumFoldr :: [Integer] -> Integer
sumFoldr = foldr (+) 0

-- elem

elemRecExpl :: Eq a => a -> [a] -> Bool 
elemRecExpl _ [] = False
elemRecExpl n (x:xs) = (n == x) || elemRecExpl n xs

elemFoldr :: Eq a => a -> [a] -> Bool 
elemFoldr n = foldr (\x rec -> x == n || rec) False

elemVersionJoni n = foldr ((||) . (== n)) False
    {-
        Recuerdo:
            (.) :: (b -> c) -> (a -> b) -> a -> c
            En este caso a seria un Integer, b y c son de tipo Bool
    -}

-- (++)

concatRecExpl :: [a] -> [a] -> [a]
concatRecExpl xs [] = xs
concatRecExpl [] ys = ys
concatRecExpl (x:xs) ys = x : concatRecExpl xs ys

-- mi acumulador va a ser ys y voy a ir concatenandole los elems de xs "por delante"
concatFoldr :: [a] -> [a] -> [a]
concatFoldr xs ys = foldr (\x rec -> x : rec) ys xs

-- filter

filterRecExpl :: (a -> Bool) -> [a] -> [a]
filterRecExpl _ [] = []
filterRecExpl p (x:xs) = if p x then x : rec else rec
    where rec = filterRecExpl p xs

filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr p = foldr (\x rec -> if p x then x : rec else rec) []

-- map

mapRecExpl :: (a -> b) -> [a] -> [b]
mapRecExpl _ [] = []
mapRecExpl f (x:xs) = f x : mapRecExpl f xs

mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f = foldr (\x rec -> f x : rec) []