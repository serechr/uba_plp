-- Ejercicio 1

max2 :: (Float, Float) -> Float
max2 (x, y) = if (x >= y) then x else y

max2' :: Float -> (Float -> Float)
max2' x y = if x > y then x else y

normaVectorial :: (Float, Float) -> Float
normaVectorial (x, y) = sqrt (x^2 + y^2)

normaVectorial' :: Float -> (Float -> Float)
normaVectorial' x y = sqrt (x^2 + y^2) -- esta raro

-- subtract' :: Float -> Float -> Float (segun la terminal son Integer)
subtract' = flip (-)

predecesor :: Integer -> Integer
predecesor = subtract 1

evaluarEnCero :: (Integer -> t) -> t
evaluarEnCero = \f -> f 0

dosVeces :: (a -> a) -> a -> a
dosVeces = \f -> f . f

-- map :: (a -> b) -> [a] -> [b]
-- flip :: (a -> b -> c) -> b -> a -> c

flipAll :: [a -> b -> c] -> [b -> a -> c] 
flipAll = map flip

flipRaro :: b -> (a -> b -> c) -> a -> c
flipRaro = flip flip

func1 = flipAll [(-), mod, div]

-- flipAllTest: map (\f -> f 2 10) func1
-- retorna [8,0,5]