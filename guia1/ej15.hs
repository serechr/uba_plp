-- Ejercicio 15

data RoseTree a = Rose a [RoseTree a]

foldRT :: (a -> [b] -> b) -> RoseTree a -> b
foldRT fRose (Rose r rts) = fRose r (map (foldRT fRose) rts)

hojas :: RoseTree a -> [a]
hojas = foldRT (\r rec -> if null rec then [r] else concat rec) 

distancias :: RoseTree a -> [Int]
distancias = undefined