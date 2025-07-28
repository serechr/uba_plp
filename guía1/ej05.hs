-- Ejercicio 5

-- Recursión estructural: permite acceder a los argumentos no recursivos de los constructores, y a los resultados de la recursión para las subestructuras.

elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x:xs) = if null xs
                                        then [x]
                                        else x : elementosEnPosicionesPares (tail xs)

{- 
    No es recursión estructural ya que está modificando la lista sobre la que hace recursión, 
    no hace recursión sobre la cola de la lista que toma. (medio dudosa la explicación, ver mejor lo de abajo).
    
    Respuesta del apunte "Recursion estructural, primitiva y global" de la sección Útil
    La recursión de elementosEnPosicionesPares es global, ya que accede a un resultado anterior: 
    el de la recursión sobre la cola de la cola de la lista (es decir tail xs).
-}

entrelazar :: [a] -> [a] -> [a]
entrelazar [] = id
entrelazar (x:xs) = \ys -> if null ys
                            then x : entrelazar xs []
                            else x : head ys : entrelazar xs (tail ys)

{-
    Sí es recursion estructural ya que el llamado recursivo de la función siempre se hace sobre la cola de la lista 
    y en ningún momento se accede a la subestructura de la lista.
-}

entrelazarFoldr :: [a] -> [a] -> [a]
entrelazarFoldr = foldr (\x f ys -> if null ys then x : f [] else x : head ys : f (tail ys)) id