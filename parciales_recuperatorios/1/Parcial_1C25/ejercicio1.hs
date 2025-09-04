data ABNV a = Hoja a 
            | Uni a (ABNV a) 
            | Bi (ABNV a) a (ABNV a)

foldABNV :: (a -> b) -> (a -> b -> b) -> (b -> a -> b -> b) -> ABNV a -> b
foldABNV fHoja fUni fBi (Hoja a) = fHoja a
foldABNV fHoja fUni fBi (Uni a r) = fUni a (foldABNV fHoja fUni fBi r)
foldABNV fHoja fUni fBi (Bi i r d) = fBi (foldABNV fHoja fUni fBi i) r (foldABNV fHoja fUni fBi d)

recABNV :: (a -> b) -> (a -> ABNV a -> b -> b) -> (ABNV a -> ABNV a -> b -> a -> b -> b) -> ABNV a -> b
recABNV fHoja fUni fBi (Hoja a) = fHoja a
recABNV fHoja fUni fBi (Uni a r) = fUni a r (recABNV fHoja fUni fBi r)
recABNV fHoja fUni fBi (Bi i r d) = fBi i d (recABNV fHoja fUni fBi i) r (recABNV fHoja fUni fBi d)

elemABNV :: Eq a => a -> ABNV a -> Bool
elemABNV n = foldABNV 
                (\x     -> n == x) 
                (\r d   -> r == n || d) 
                (\i r d -> r == n || i || d)

reemplazarUno :: Eq a => a -> a -> ABNV a -> ABNV a
reemplazarUno x y = recABNV (\h               -> if (h == x) then (Hoja y) else Hoja h) 
                            (\a r recr        -> if (a == x) then (Uni y r) else (Uni a recr)) 
                            (\i d reci r recd -> if (r == x) then (Bi i y d) else (if (elemABNV x i) then (Bi reci r d) else (Bi i r recd)))

-- REVISAR reemplazarUno, ver bien el orden de los parametros que le paso a recABNV
-- sale con recursion primitiva porque tengo que usar elemABNV y estaria accediendo a la subestructura

nivel :: ABNV a -> Int -> [a]
nivel a = foldABNV (\x n     -> if (n == 0) then [x] else []) 
                   (\a r n   -> if (n == 0) then [a] else r (n - 1)) 
                   (\i a d n -> if (n == 0) then [a] else i (n - 1) ++ d (n - 1)) a

abnv1 = Bi (Bi (Hoja 9) 7 (Hoja 5)) 3 (Uni 3 (Hoja 4))
abnv2 = Bi (Hoja 1) 2 (Uni 3 (Hoja 4))