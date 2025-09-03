-- Ejercicio 12

data AB a = Nil | Bin (AB a) a (AB a)

-- Usando recursión explícita, definir los esquemas de recursión estructural (foldAB) y primitiva (recAB), y dar sus tipos.

foldAB :: b -> (b -> a -> b -> b) -> AB a -> b
foldAB cNil cBin arb = case arb of
                        Nil       -> cNil
                        Bin i r d -> cBin (rec i) r (rec d)
                where
                        rec = foldAB cNil cBin

recAB :: b -> (AB a -> b -> a -> AB a -> b -> b) -> AB a -> b
recAB cNil cBin arb = case arb of
                        Nil       -> cNil
                        Bin i r d -> cBin i (rec i) r d (rec d)
                where
                        rec = recAB cNil cBin

-- Definir las funciones esNil, altura y cantNodos (para esNil puede utilizarse case en lugar de foldAB o recAB).

esNil :: AB a -> Bool
esNil arb = case arb of
        Nil -> True
        _   -> False

altura :: AB a -> Int
altura = foldAB 0 (\reci _ recd -> 1 + max reci recd)

cantNodos :: AB a -> Int
cantNodos = foldAB 0 (\reci _ recd -> 1 + reci + recd)

{- Definir la función mejorSegún :: (a -> a -> Bool) -> AB a -> a, análoga a la del ejercicio 3, para árboles.
   Se recomienda definir una función auxiliar para comparar la raíz con un posible resultado de la recursión para un árbol que puede o no ser Nil. 
   
   Recuerdo lo del ejercicio 3:
        mejorSegunRecExpl :: (a -> a -> Bool) -> [a] -> a
        mejorSegunRecExpl _ [x] = x
        mejorSegunRecExpl p (x:y:xs) = if p x y then mejorSegunRecExpl p (x:xs)
                                                else mejorSegunRecExpl p (y:xs)

        -- foldr1: it takes the last two items of the list and applies the function, then it takes the third item from the end and the result, and so on.

        mejorSegun :: (a -> a -> Bool) -> [a] -> a
        mejorSegun p = foldr1 (\x y -> if p x y then x else y)
   
   -}

mejorSegun :: (a -> a -> Bool) -> AB a -> a
mejorSegun f = undefined

arb1 :: AB Integer
arb1 = Bin Nil 8 (Bin (Bin (Bin Nil 5 Nil) 4 (Bin Nil 7 Nil)) 9 Nil)