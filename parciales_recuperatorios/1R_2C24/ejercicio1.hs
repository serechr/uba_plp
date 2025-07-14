data Operador = Suma Int | DividirPor Int | Secuencia [Operador]

foldOperador :: (Int -> b) -> (Int -> b) -> ([b] -> b) -> Operador -> b
foldOperador fSuma fDividirPor fSecuencia (Suma n) = fSuma n
foldOperador fSuma fDividirPor fSecuencia (DividirPor n) = fDividirPor n
foldOperador fSuma fDividirPor fSecuencia (Secuencia xs) = fSecuencia (map (foldOperador fSuma fDividirPor fSecuencia) xs)

falla :: Operador -> Bool
falla = foldOperador 
            (\_   -> False)
            (\n   -> n == 0)
            (or)

-- REVISAR: intentar hacer la función de fSec de falla con una lambda, no me termina de cerrar por qué el or así solito funca
-- se puede hacer recursión con una función lambda????

aplanar :: Operador -> Operador
aplanar = foldOperador 
            (\n   -> Suma n)
            (\n   -> DividirPor n)
            (\ops -> Secuencia (concatMap f ops))
        where
            f x = case x of
                    Secuencia ops -> ops
                    op            -> [op]

-- REVISAR: darle un nombre a f más declarativo

aplanar2 :: Operador -> Operador
aplanar2 = foldOperador Suma DividirPor (Secuencia . concatMap (\x -> case x of
                                                                        Suma n       -> [x]
                                                                        DividirPor n -> [x]
                                                                        Secuencia xs -> xs))

componerTodas :: [a -> a] -> (a -> a)
componerTodas = foldl (.) id

aplicar :: Operador -> Int -> Maybe Int
aplicar op n = if falla (aplanar op) then Nothing
                                     else Just ((componerTodas (listaOpAFunc (aplanar op))) n)

aplicarOp :: Operador -> (Int -> Int)
aplicarOp op = case op of 
                    Suma x       -> (+) x
                    DividirPor x -> (`div` x)

listaOpAFunc :: Operador -> [Int -> Int]
listaOpAFunc (Secuencia xs) = map aplicarOp xs

-- aplicar está mal, no devuelve lo que debería. REVISAR bien con Cami

op1 = Secuencia [Suma 3, DividirPor 2, Secuencia [Secuencia [Suma 5, DividirPor 0]]]
op2 = Secuencia [Suma 3, DividirPor 2, Secuencia [Secuencia [Suma 5, DividirPor 4]]]
op3 = Secuencia [Suma 5, DividirPor 2]
op4 = Secuencia [Suma 5, DividirPor 0]
