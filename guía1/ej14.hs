-- Ejercicio 14

data AIH a = Hoja a | Bin (AIH a) (AIH a)

foldAIH :: (a -> b) -> (b -> b -> b) -> AIH a -> b
foldAIH fHoja fBin arb = case arb of
                        Hoja r  -> fHoja r
                        Bin i d -> fBin (rec i) (rec d)
        where
            rec = foldAIH fHoja fBin

altura :: AIH a -> Integer
altura = foldAIH (const 1) (\reci recd -> 1 + max reci recd)

tamaño :: AIH a -> Integer
tamaño = foldAIH (const 1) (\reci recd -> reci + recd) -- en el caso Bin en vez de la lambda podemos dejar solo (+)

arb1 = Bin (Hoja 5) (Bin (Hoja 4) (Bin (Bin (Hoja 2) (Hoja 3)) (Hoja 1)))
arb2 = Bin (Hoja 4) (Bin (Hoja 3) (Bin (Hoja 2) (Hoja 1)))
arb3 = Hoja 1