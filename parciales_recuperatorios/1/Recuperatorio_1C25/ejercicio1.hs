import Data.List -- para poder usar nub

data LineaProd = Materiales [String] 
               | Agregar String LineaProd 
               | Unir LineaProd LineaProd



foldLineaProd :: ([String] -> b) -> (String -> b -> b) -> (b -> b -> b) -> LineaProd -> b
foldLineaProd fMateriales fAgregar fUnir lp = case lp of
                                                Materiales xs -> fMateriales xs
                                                Agregar s lp  -> fAgregar s (rec lp)
                                                Unir lp1 lp2  -> fUnir (rec lp1) (rec lp2)
                where 
                    rec = foldLineaProd fMateriales fAgregar fUnir

recLineaProd :: ([String] -> b) -> (String -> b -> LineaProd -> b) -> (b -> b -> LineaProd -> LineaProd -> b) -> LineaProd -> b
recLineaProd fMateriales fAgregar fUnir lp = case lp of
                            Materiales xs  -> fMateriales xs
                            Agregar s lp   -> fAgregar s (rec lp) lp
                            Unir lp1 lp2   -> fUnir (rec lp1) (rec lp2) lp1 lp2
                where 
                    rec = recLineaProd fMateriales fAgregar fUnir


materialesUsados :: LineaProd -> [String]
materialesUsados lp = nub (foldLineaProd id (:) (++) lp)

sublineasDisjuntas :: LineaProd -> Bool
sublineasDisjuntas = recLineaProd 
                            (const True) 
                            (\_ rlp _ -> rlp) 
                            (\rlp1 rlp2 lp1 lp2 -> null (intersect (materialesUsados lp1) (materialesUsados lp2)) && rlp1 && rlp2)

mismaEstructura :: LineaProd -> LineaProd -> Bool
mismaEstructura = foldLineaProd (\_ lp2 -> case lp2 of 
                                            Materiales _ -> True
                                            _ -> False)
                                (\s rlp lp2 -> case lp2 of 
                                            Agregar _ rlp2 -> rlp rlp2
                                            _ -> False)
                                (\rlp1 rlp2 lp2 -> case lp2 of 
                                            Unir rlp21 rlp22 -> rlp1 rlp21 && rlp2 rlp22
                                            _ -> False)

l1 = Unir (Materiales ["acero", "cobre"]) (Unir (Agregar "madera" (Materiales ["madera"])) (Materiales ["mercurio"]))
l2 = Unir (Materiales ["acero", "cobre"]) (Unir (Agregar "madera" (Materiales ["aluminio"])) (Agregar "aluminio" (Materiales ["plastico"])))
l3 = Unir (Materiales ["m1"]) (Unir (Agregar "m3" (Materiales ["m3", "m4"])) (Materiales ["m1","m2"]))