-- Ejercicio 11

-- Definir el esquema de recursión estructural para el siguiente tipo:
data Polinomio a = X
                 | Cte a
                 | Suma (Polinomio a) (Polinomio a)
                 | Prod (Polinomio a) (Polinomio a)

foldPoli :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Polinomio a -> b
foldPoli fX fCte fSuma fProd poli = case poli of
                                        X          -> fX
                                        Cte p      -> fCte p
                                        Suma p1 p2 -> fSuma (rec p1) (rec p2)
                                        Prod p1 p2 -> fProd (rec p1) (rec p2)
                            where
                                rec = foldPoli fX fCte fSuma fProd

{-  Luego usar el esquema definido para escribir la función evaluar :: Num a => a -> Polinomio a -> a que, dado un número y un polinomio, 
    devuelve el resultado de evaluar el polinomio dado en el número dado. -}

evaluar :: Num a => a -> Polinomio a -> a
evaluar n = foldPoli n id (+) (*)