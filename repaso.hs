{-
Haskell toma los paramétros uno a uno. 

Una función está currificada si puede tomar los paramétros uno a uno y no en forma de tupla. Con una función currificada podemos escribir más funciones gracias a la aplicación parcial.

curry :: ((a,b) -> c) -> (a -> b -> c)
uncurry :: (a -> b -> c) -> ((a,b) -> c)

-}

comp :: (b -> c) -> (a -> b) -> a -> c
comp f g a = f (g a)

flip' :: (a -> b -> c) -> b -> a -> c
flip' f a b = f b a

peso :: (a -> b) -> a -> b
peso f a = f a

const' :: a -> b -> a
const' a b = a

{-
Macros para definir listas:

Por extensión
Secuencias
Por comprensión: [expresión | selectores, condiciones]

También podemos trabajar con listas infinitas: [1..]
-}

sumanCuatro = [(x,y) | x <- [0, 2..4], y <- [0..4], x+y == 4]

-- Una función parcial es una función que sólo se define para algunas entradas entonces puede tener salidas no válidas para otros valores

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun _ [x] = x
mejorSegun f (x:y:ys) = if f x y then mejorSegun f (x:ys) else mejorSegun f (y:ys)

-- filter :: (a -> Bool) -> [a] -> [a]
-- map :: (a -> b) -> [a] -> [b]

reverseAnidado :: [[Char]] -> [[Char]]
reverseAnidado xs = reverse (map reverse xs)

paresCuadrados :: [Int] -> [Int]
paresCuadrados = map (\x -> if mod x 2 == 0 then x*x else x)

{-
foldr (a -> b -> b) -> b -> [a] -> b
foldr _ z [] = z
foldr f z (x:xs) = f x (foldr f x xs)
-}


filter' :: (a -> Bool) -> [a] -> [a]
