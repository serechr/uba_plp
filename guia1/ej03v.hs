{-
    Hacer lo mismo que en el punto anterior, pero en sentido inverso (el último elemento menos el anteúltimo, etc.). 
    Pensar qué esquema de recursión conviene usar en este caso.

    foldl :: (b -> a -> b) -> b -> [a] -> b
    foldl _ z []     = z                   -- Caso base
    foldl f z (x:xs) = foldl f (f z x) xs  -- Paso recursivo

-}

sumaAlt1 :: Num a => [a] -> a -- este es el del ejercicio anterior
sumaAlt1 = foldr (-) 0

-- diferentes versiones de lo mismo:

sumaAlt :: Num a => [a] -> a
sumaAlt = sumaAlt1 . reverse

sumaAltFoldl :: Num a => [a] -> a
sumaAltFoldl = foldl (flip (-)) 0

sumaAltFoldlLambda :: Num a => [a] -> a
sumaAltFoldlLambda = foldl (\acc x -> x - acc) 0