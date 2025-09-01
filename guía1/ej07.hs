-- Ejercicio 7

-- Definir las siguientes funciones para trabajar sobre listas, y dar su tipo. Todas ellas deben poder aplicarse a listas finitas e infinitas.

{-  mapPares, una versión de map que toma una función currificada de dos argumentos y una lista de pares
    de valores, y devuelve la lista de aplicaciones de la función a cada par. Pista: recordar curry y uncurry. -}

mapPares :: (a -> b -> c) -> [(a, b)] -> [c]
mapPares f = map (uncurry f)

{-  armarPares, que dadas dos listas arma una lista de pares que contiene, en cada posición, el elemento
    correspondiente a esa posición en cada una de las listas. Si una de las listas es más larga que la otra,
    ignorar los elementos que sobran (el resultado tendrá la longitud de la lista más corta). Esta función en
    Haskell se llama zip. Pista: aprovechar la currificación y utilizar evaluación parcial. -}

armarPares :: [a] -> [b] -> [(a, b)]
armarPares = foldr (\x rec ys -> if null ys then [] else (x, head ys) : rec (tail ys)) (const [])

{-  mapDoble, una variante de mapPares, que toma una función currificada de dos argumentos y dos listas
    (de igual longitud), y devuelve una lista de aplicaciones de la función a cada elemento correspondiente de
    las dos listas. Esta función en Haskell se llama zipWith. -}

mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble f xs ys = mapPares f (armarPares xs ys)