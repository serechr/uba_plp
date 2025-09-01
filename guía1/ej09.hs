-- Ejercicio 9

{-  Definir y dar el tipo del esquema de recursión foldNat sobre los naturales. Utilizar el tipo Integer de Haskell 
    (la función va a estar definida sólo para los enteros mayores o iguales que 0). -}

data Nat = Zero | Succ Nat

foldNat' cZero cSucc Zero = cZero
foldNat' cZero cSucc (Succ n) = cSucc (rec n)
    where rec = foldNat' cZero cSucc

-- foldNat :: Num a => (a -> b -> b) -> b -> a -> b
foldNat f z 0 = z
foldNat f z n = f n (foldNat f z (n-1))

--  Utilizando foldNat, definir la función potencia.

potencia :: (Eq a, Num a, Num b) => b -> a -> b
potencia x = foldNat (\_ rec -> rec * x) 1