-- Ejercicio 5

data AB a = Nil | Bin (AB a) a (AB a)

vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB _ = False

vacioAB2 :: AB a -> Bool
vacioAB2 Nil = True
vacioAB2 (Bin _ _ _) = False

negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin i r d) = Bin (negacionAB i) (not r) (negacionAB d) 

productoAB :: AB Int -> Int
productoAB Nil = 1
productoAB (Bin i r d) = (productoAB i) * r * (productoAB d)