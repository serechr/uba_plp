import Data.List

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
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

-- Ejercicios de parciales

-- 1C24 (Parcial)

data AT a = NilT | Tri a (AT a) (AT a) (AT a)

foldAT :: b -> (a -> b -> b -> b -> b) -> AT a -> b
foldAT fNilT fTri t = case t of 
            NilT -> fNilT
            Tri r i m d -> fTri r (rec i) (rec m) (rec d)
    where
        rec = foldAT fNilT fTri

preorder :: AT a -> [a]
preorder = foldAT [] (\r ri rm rd -> r:(ri ++ rm ++ rd))

mapAT :: (a -> b) -> AT a -> AT b
mapAT f = foldAT NilT (\x ri rm rd -> Tri (f x) ri rm rd)

nivel :: AT a -> Int -> [a]
nivel = foldAT (\_ -> []) (\x ri rm rd m -> if m == 0 then [x] else (ri (-1) ++ rm (-1) ++ rd (-1)))


-- 1C24 (Recu)

data Prop = Var String | No Prop | Y Prop Prop | O Prop Prop | Imp Prop Prop

type Valuación = String -> Bool

foldProp :: (String -> b) -> (b -> b) -> (b -> b -> b) -> (b -> b -> b) -> (b -> b -> b) -> Prop -> b
foldProp fVar fNo fY fO fImp p = case p of
                    Var s -> fVar s
                    No p -> fNo (rec p)
                    Y p1 p2 -> fY (rec p1) (rec p2)
                    O p1 p2 -> fO (rec p1) (rec p2)
                    Imp p1 p2 -> fImp (rec p1) (rec p2)
        where
            rec = foldProp fVar fNo fY fO fImp

recProp :: (String -> b) -> (Prop -> b -> b) -> (Prop -> Prop -> b -> b -> b) -> (Prop -> Prop -> b -> b -> b) -> (Prop -> Prop -> b -> b -> b) -> Prop -> b
recProp fVar fNo fY fO fImp p = case p of 
                    Var s -> fVar s
                    No p -> fNo p (rec p)
                    Y p1 p2 -> fY p1 p2 (rec p1) (rec p2)
                    O p1 p2 -> fO p1 p2 (rec p1) (rec p2)
                    Imp p1 p2 -> fImp p1 p2 (rec p1) (rec p2)
        where
            rec = recProp fVar fNo fY fO fImp

variables :: Prop -> [String]
variables p = nub (foldProp (\s -> s) (++) (++) (++)) 