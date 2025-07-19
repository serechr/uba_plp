-- Ejercicio 6

-- recursión primitiva
recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

sacarUna :: Eq a => a -> [a] -> [a]
sacarUna n xs = recr (\x xs r -> if x == n then xs else x : r) [] xs

insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado n xs = recr (\x xs r -> if n > x then x : r else n : x : xs) [] xs
