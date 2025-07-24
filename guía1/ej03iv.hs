{-
    Definir la función sumaAlt, que realiza la suma alternada de los elementos de una lista. Es decir, da como
    resultado: el primer elemento, menos el segundo, más el tercero, menos el cuarto, etc. Usar foldr.

    La clave de este ejercicio está en ver que:
        1 - (2 - (3 - (4 - 5))) = 1 + (-2 + (3 + (-4 + 5)))
-}

sumaAltRecExpl :: Num a => [a] -> a
sumaAltRecExpl []     = 0
sumaAltRecExpl (x:xs) = x - sumaAltRecExpl xs

sumaAlt :: Num a => [a] -> a
sumaAlt xs = foldr (\x acc -> (-) x acc) 0 xs

sumaAlt2 :: Num a => [a] -> a
sumaAlt2 = foldr (\x acc -> x - acc) 0

sumaAlt3 :: Num a => [a] -> a
sumaAlt3 = foldr (-) 0

{- (mucho texto, lo importante está más al final)

      foldr :: (a -> b -> b) -> b -> t a -> b
{F0}  foldr f z []     = z
{F1}  foldr f z (x:xs) = f x (foldr f z xs)

Veamoslo en un ejemplo paso a paso:
    xs = [1,2,3,4,5]
    
    sumaAlt [1,2,3,4,5] = foldr (\x acc -> (-) x acc) 0 [1,2,3,4,5]
{F1}    = (\x acc -> (-) x acc) 1 (foldr (\x acc -> (-) x acc) 0 [2,3,4,5])
{F1}    = (\x acc -> (-) x acc) 1 ((\x acc -> (-) x acc) 2 (foldr (\x acc -> (-) x acc) 0 [3,4,5]))
{F1}    = (\x acc -> (-) x acc) 1 ((\x acc -> (-) x acc) 2 ((\x acc -> (-) x acc) 3 (foldr (\x acc -> (-) x acc) 0 [4,5])))
{F1}    = (\x acc -> (-) x acc) 1 ((\x acc -> (-) x acc) 2 ((\x acc -> (-) x acc) 3 ((\x acc -> (-) x acc) 4 (foldr (\x acc -> (-) x acc) 0 [5]))))
{F1}    = (\x acc -> (-) x acc) 1 ((\x acc -> (-) x acc) 2 ((\x acc -> (-) x acc) 3 ((\x acc -> (-) x acc) 4 ((\x acc -> (-) x acc) 5 (foldr (\x acc -> (-) x acc) 0 [])))))
{F0}    = (\x acc -> (-) x acc) 1 ((\x acc -> (-) x acc) 2 ((\x acc -> (-) x acc) 3 ((\x acc -> (-) x acc) 4 ((\x acc -> (-) x acc) 5 0))))
{beta}  = (\x acc -> (-) x acc) 1 ((\x acc -> (-) x acc) 2 ((\x acc -> (-) x acc) 3 ((\x acc -> (-) x acc) 4 ((-) 5 0))))
{beta}  = (\x acc -> (-) x acc) 1 ((\x acc -> (-) x acc) 2 ((\x acc -> (-) x acc) 3 ((-) 4 ((-) 5 0))))
{beta}  = (\x acc -> (-) x acc) 1 ((\x acc -> (-) x acc) 2 (3 ((-) 4 ((-) 5 0))))
{beta}  = (\x acc -> (-) x acc) 1 ((-) 2 ((-) 3 ((-) 4 ((-) 5 0))))
{beta}  = (-) 1 ((-) 2 ((-) 3 ((-) 4 ((-) 5 0))))

Queda ir aplicando cada resta nomás pero la idea es que vamos a tener algo así:
    1 - (2 - (3 - (4 - 5)))
que si distribuimos el signo negativo, eso va a ser igual a:
    1 + (-2 + (3 + (-4 + 5)))
Vemos que los numeros que estan en posiciones impares (arrancando desde 0, o sea los elementos 2 y 4) van a terminar siendo negativo o sea los restamos,
mientras que, los elementos en posiciones pares (1,3,5, justo los elementos impares están en posiciones pares :/) los sumamos
-}