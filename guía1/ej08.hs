-- Ejercicio 8

{-  Escribir la función sumaMat, que representa la suma de matrices, usando zipWith. Representaremos una
    matriz como la lista de sus filas. Esto quiere decir que cada matriz será una lista finita de listas finitas,
    todas de la misma longitud, con elementos enteros. Recordamos que la suma de matrices se define como la suma celda a celda. 
    Asumir que las dos matrices a sumar están bien formadas y tienen las mismas dimensiones. -}

sumaMat :: [[Int]] -> [[Int]] -> [[Int]]
sumaMat = zipWith (zipWith (+))

{-  Escribir la función trasponer, que, dada una matriz como las del ítem i, devuelva su traspuesta. Es decir,
    en la posición i, j del resultado está el contenido de la posición j, i de la matriz original. Notar que si la
    entrada es una lista de N listas, todas de longitud M, la salida debe tener M listas, todas de longitud N. -}

trasponer :: [[Int]] -> [[Int]]
trasponer = undefined