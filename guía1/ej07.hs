-- Ejercicio 7

-- mapPares :: (a -> a -> b) -> [(a,a)] -> b, preguntar esto
mapPares f = map (uncurry f)

armarPares :: [a] -> [b] -> [(a, b)]
armarPares = foldr (\x rec ys -> if null ys then [] else (x, head ys) : rec (tail ys)) (const [])

-- mapDoble :: (a -> b1 -> b2) -> [a] -> [b1] -> [b2]
mapDoble f xs ys = mapPares f (armarPares xs ys)
