-- Ejercicio 6

-- Recursión primitiva
recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

{- 
    Definir la función sacarUna :: Eq a => a -> [a] -> [a], que dados un elemento y una lista devuelve el
    resultado de eliminar de la lista la primera aparición del elemento (si está presente). 
-}

sacarUna :: Eq a => a -> [a] -> [a]
sacarUna n xs = recr (\x xs r -> if x == n then xs else x : r) [] xs

{- 
    Explicar por qué el esquema de recursión estructural (foldr) no es adecuado para implementar la función sacarUna del punto anterior. 

    Necesitamos acceder a la subestructura para poder conservarla tal cual está al momento en el que encontramos el elemento que queremos sacar.
    No podríamos usar foldr ya que no tenemos forma de acceder a la subestructura, la recursión no pararía nunca y sacaría todas las apariciones
    del elemento que deseamos sacar. -}

{- 
    Definir la función insertarOrdenado :: Ord a => a -> [a] -> [a] que inserta un elemento en una lista ordenada (de manera creciente), 
    de manera que se preserva el ordenamiento. 
-}

insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado n xs = recr (\x xs r -> if n > x then x : r else n : x : xs) [] xs