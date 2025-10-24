-- Ejercicio 10

{-  Definir la función genLista :: a -> (a -> a) -> Integer -> [a], que genera una lista de una cantidad dada de elementos, 
    a partir de un elemento inicial y de una función de incremento entre los elementos de la lista. Dicha función de incremento, 
    dado un elemento de la lista, devuelve el elemento siguiente. -}

genLista :: a -> (a -> a) -> Integer -> [a]
genLista elemInicial _ 1 = [elemInicial]
genLista elemInicial fIncr longitud = genLista elemInicial fIncr (longitud - 1) ++ [fIncr (last (genLista elemInicial fIncr (longitud - 1)))]


-- versión super pro minimalista
genLista2 :: a -> (a -> a) -> Int -> [a]
genLista2 e f n = take n (iterate f e)

{-  Usando genLista, definir la función desdeHasta, que dado un par de números (el primero menor que el segundo), 
    devuelve una lista de números consecutivos desde el primero hasta el segundo. -}

desdeHasta :: Int -> Integer -> [Int]
desdeHasta x y = genLista x (+1) y

-- aplicamos eta-reducción y sacamos el parámetro y de ambos lados
desdeHasta2 :: Int -> Integer -> [Int]
desdeHasta2 x = genLista x (+1)