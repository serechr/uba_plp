-- Ejercicio 2

-- curry toma una función que recibe una tupla y la convierte en una función que recibe parámetros uno en uno

curry1 :: ((a,b) -> c) -> a -> b -> c
curry1 f x y = f (x,y)

curry2 f = \x y -> f (x,y)

curry3 f = \x -> (\y -> f (x, y))

----

-- uncurry toma una función que recibe los parámetros uno en uno y la convierte en una función que recibe una tupla

uncurry1 :: (a -> b -> c) -> (a,b) -> c
uncurry1 f (x,y) = f x y

uncurry2 f = \(x,y) -> f x y

-- todas hacen lo mismo pero está bueno verlo de diferentes formas :)