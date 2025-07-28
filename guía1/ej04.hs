{-
    Definir la función permutaciones :: [a] -> [[a]], que dada una lista devuelve todas sus permutaciones. 
    Se recomienda utilizar concatMap :: (a -> [b]) -> [a] -> [b], y también take y drop.
-}

insertarElem :: a -> [a] -> [[a]]
insertarElem x xs = [take i xs ++ [x] ++ drop i xs | i <- [0.. length xs]]

permutacionesRecExpl :: [a] -> [[a]]
permutacionesRecExpl [] = [[]]
permutacionesRecExpl (x:xs) = concatMap (insertarElem x) (permutacionesRecExpl xs)

permutaciones :: [a] -> [[a]]
permutaciones = foldr (concatMap . insertarElem) [[]]

{-
    Definir la función partes, que recibe una lista L y devuelve la lista de todas las listas formadas por los
    mismos elementos de L, en su mismo orden de aparición.
    Ejemplo: partes [5, 1, 2] → [[], [5], [1], [2], [5, 1], [5, 2], [1, 2], [5, 1, 2]] (en algún orden).
-}

subconjN :: Int -> [a] -> [[a]]
subconjN 0 _ = [[]]
subconjN n [] = []
subconjN n (x:xs) = (map (x:) (subconjN (n-1) xs)) ++ subconjN n xs

partes :: [a] -> [[a]]
partes xs = concat [subconjN n xs | n <- [0..length xs]]

{-
    Definir la función prefijos, que dada una lista, devuelve todos sus prefijos.
    Ejemplo: prefijos [5, 1, 2] → [[], [5], [5, 1], [5, 1, 2]]
-}

prefijos :: [a] -> [[a]]
prefijos xs = [take i xs | i <- [0.. length xs]]

{-
    Definir la función sublistas que, dada una lista, devuelve todas sus sublistas 
    (listas de elementos que aparecen consecutivos en la lista original).
    Ejemplo: sublistas [5, 1, 2] → [[], [5], [1], [2], [5, 1], [1, 2], [5, 1, 2]] (en algún orden).
-}

sublistasN :: Int -> [a] -> [[a]]
sublistasN _ [] = []
sublistasN 0 xs = [[]]
sublistasN n xs = [take n (drop i xs) | i <- [0..length xs - n]]

sublistas :: [a] -> [[a]]
sublistas xs = concat [sublistasN n xs | n <- [0..length xs]]