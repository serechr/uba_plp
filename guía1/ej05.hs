-- Ejercicio 5

elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x:xs) = if null xs
                                        then [x]
                                        else x : elementosEnPosicionesPares (tail xs)

    {- no es recursión estructural ya que está modificando la lista sobre la que hace recursión, 
    no hace recursión sobre la cola de la lista que toma -}

-- repasar ejercicio entrelazar

entrelazar :: [a] -> [a] -> [a]
entrelazar [] = id
entrelazar (x:xs) = \ys -> if null ys
                            then x : entrelazar xs []
                            else x : head ys : entrelazar xs (tail ys)

entrelazarFoldr :: [a] -> [a] -> [a]
entrelazarFoldr = foldr (\x f ys -> if null ys then x : f [] else x : head ys : f (tail ys)) id
