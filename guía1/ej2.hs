-- Ejercicio 2

-- curry toma una función que recibe una tupla y la convierte en una función que recibe parámetros uno en uno
-- uncurry toma una función que recibe los parámetros uno en uno y la convierte en una función que recibe una tupla

-- curry1 :: ((a,b) -> c) -> a -> b -> c
-- curry f = \x -> (\y -> f (x, y))
curry1 f x y = f (x,y)
curry2 f = \x y -> f (x,y)

-- uncurry1 :: (a -> b -> c) -> (a,b) -> c
-- uncurry f = \(x,y) -> f x y
uncurry1 f (x,y) = f x y
uncurry2 f = \(x,y) -> f x y
