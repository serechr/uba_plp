-- Ejercicio 9
data Nat = Zero | Succ Nat

foldNat' cZero cSucc Zero = cZero
foldNat' cZero cSucc (Succ n) = cSucc (rec n)
    where rec = foldNat' cZero cSucc

-- foldNat :: Num a => (a -> b -> b) -> b -> a -> b
foldNat f z 0 = z
foldNat f z n = f n (foldNat f z (n-1))

-- potencia :: a -> b -> a
potencia x = foldNat (\_ rec -> rec * x) 1