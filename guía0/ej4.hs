-- Ejercicio 4

limpiar :: String -> String -> String
limpiar [] ys = ys
limpiar (x:xs) ys = limpiar xs (filter (/= x) ys)

longitud :: [t] -> Float
longitud [] = 0
longitud (x:xs) = 1 + longitud (xs)

promedio :: [Float] -> Float
promedio xs = sum(xs) / longitud(xs)

difPromedio :: [Float] -> [Float] 
difPromedio xs = map (\x -> x - promedio xs) xs

todosIguales1 :: [Int] -> Bool
todosIguales1 [] = True
todosIguales1 [x] = True
todosIguales1 (x:y:xs) = x == y && todosIguales1 xs

todosIguales2 :: [Int] -> Bool
todosIguales2 [] = True
todosIguales2 (x:xs) = all (== x) xs

todosIguales3 :: [Int] -> Bool
todosIguales3 [] = True
todosIguales3 (x:xs) = length (filter (\y -> y == x) (x:xs)) == length (x:xs)