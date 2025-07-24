{-
    Definir la función sumasParciales :: Num a => [a] -> [a], que dada una lista de números devuelve otra de la misma longitud, 
    que tiene en cada posición la suma parcial de los elementos de la lista original desde la cabeza hasta la posición actual. 
    Por ejemplo, sumasParciales [1,4,-1,0,5] --> [1,5,4,4,9]. 

    foldl :: (b -> a -> b) -> b -> [a] -> b
    foldl _ z []     = z                   -- Caso base
    foldl f z (x:xs) = foldl f (f z x) xs  -- Paso recursivo
-}

sumasParcialesRecExpl xs = sumaConAcc 0 xs
    where
        sumaConAcc _ [] = []
        sumaConAcc acc (y:ys) = (acc + y) : sumaConAcc (acc + y) ys


sumasParciales :: Num a => [a] -> [a]
sumasParciales = foldl (\ls x -> if null ls then [x] else [x + last ls]) [] -- ls sería la lista solución donde voy a ir armando mi lista 
                                                                            -- hasta terminar de recorrer todos los elementos

sumasParcialesConTupla :: Num a => [a] -> [a]
sumasParcialesConTupla = snd . foldl (\(acc, res) x -> (acc + x, res ++ [acc + x])) (0, [])