-- Ejercicio 12

data AB a = Nil | Bin (AB a) a (AB a)

foldAB :: b -> (b -> a -> b -> b) -> AB a -> b
foldAB cVacio cBin arb = case arb of 
                    Nil -> cVacio
                    Bin i r d -> cBin (rec i) r (rec d)
        where rec = foldAB cVacio cBin

-- recr :: (a -> [a] -> b -> b) -> b -> [a] -> b

-- recAB :: (a -> AB a -> b -> (b -> a -> b -> b))
recAB cVacio cBin arb = case arb of 
                Nil -> cVacio
                (Bin i r d) -> cBin 