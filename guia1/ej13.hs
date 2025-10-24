-- Ejercicio 13

data AB a = Nil | Bin (AB a) a (AB a)

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

esNil :: AB a -> Bool
esNil arb = case arb of
        Nil -> True
        _   -> False

arb1 = Bin Nil 8 (Bin (Bin (Bin Nil 5 Nil) 4 (Bin Nil 7 Nil)) 9 Nil)
arb2 = Bin Nil 11 (Bin (Bin (Bin Nil 7 Nil) 2 (Bin Nil 3 Nil)) 5 Nil)
arb3 = Bin Nil 11 (Bin (Bin (Bin Nil 7 Nil) 2 Nil) 5 Nil)

ramas :: AB a -> [[a]]
ramas = undefined

cantHojas :: AB a -> Int
cantHojas = recAB 0 (\i reci r d recd -> if (esNil i && esNil d) then 1 + reci + recd else reci + recd)

espejo :: AB a -> AB a
espejo = foldAB Nil (\reci r recd -> Bin recd r reci)

mismaEstructura :: AB a -> AB b -> Bool
mismaEstructura = foldAB esNil (\reci r recd arb -> case arb of 
                                                    Bin i _ d -> reci i && recd d
                                                    _         -> False)