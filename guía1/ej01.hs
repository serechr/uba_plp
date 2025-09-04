{- INFO

Observación: las fleECHAs de los tipos en Haskell asocian a derECHA entonces:
    suma :: Int -> Int -> Int 
también lo podemos ver como: 
    suma :: Int -> (Int -> Int)

Está bueno pensarlo como que todas las funciones en Haskell toman un sólo argumento y te pueden devolver funciones,
en este caso, suma tomaría un argumento y te devuelve una función que recibe otro argumento. 
Nosotros decidimos si pasarle un argumento a la vez o darle los dos de una.

La aplicación parcial en Haskell implica pasar menos que el número total de argumentos a una función que toma múltiples argumentos.

-}

-- Ejercicio 1

max2 :: (Float, Float) -> Float
max2 (x, y) | x >= y = x
            | otherwise = y

    -- No está currificada, al tomar los dos parámetros como tuplas nos vemos en la obligación de pasarselos, no podemos aplicar parcialmente la función.

max2' :: Float -> (Float -> Float) -- los paréntesis los podemos sacar pero es para que se vea mejor la idea de currificación
max2' x y = if (x >= y) then x else y

maximoEntreCuatro :: Float -> Float
maximoEntreCuatro = max2' 4

maximoEntreCuatro' = \y -> max2 (4, y)
----

normaVectorial :: (Float, Float) -> Float
normaVectorial (x, y) = sqrt (x^2 + y^2)

    -- No está currificada, pasa lo mismo que en max2, la función toma una tupla con ambos parámetros.

normaVectorial' :: Float -> Float -> Float
normaVectorial' x y = sqrt (x^2 + y^2)

----

subtract' :: Float -> Float -> Float
subtract' = flip (-)

    -- Sí está currificada, puede tomar los parámetros uno a la vez :)

----

predecesor :: Integer -> Integer
predecesor = subtract 1

    -- Toma un solo parámetro entonces medio que ya está currificada lo queramos o no, no podría tomar otro parámetro.

---- Aparecen las funciones lambda!!!

{- INFO

Las funciones lambda son funciones anónimas, no les damos ningún nombre. 
Las podemos usar """localmente""" junto con otras funciones como map o foldr.

Veamos algunos ejemplos:

\x -> x + 1         Recibimos un parámetro y le sumamos 1

\x y -> x + y       Sumamos dos números. Recibe los parámetros tal cual en ese orden, aplicar esta función sería algo así:
    (\x y -> x + y) 2 3 = (\y -> 2 + y) 3 = 2 + 3 = 5

Veamos un par de casos particulares:

(\x y z -> z + x - y) 2 5 3 = (\y z -> z + 2 - y) 5 3 = (\z -> z + 2 - 5) 3 = 3 + 2 - 5 = 0
Sustituciones:           x := 2                    y := 5                z := 3
    
(\x y -> 13) 8 9 = (\y -> 13) 9 = 13
Sust.:        x := 8         y := 9    Como x e y no aparecen en la función, las sustituciones técnicamente no se hacen, la función siempre devuelve 13.

Observación: las aplIcaciones asocian a Izquierda, entonces
    (\x y z -> z + x - y) 2 5 3 = (((\x y z -> z + x - y) 2) 5) 3
    (\x y -> 13) 8 9            = ((\x y -> 13) 8) 9
-}

evaluarEnCero :: (Integer -> t) -> t
evaluarEnCero = \f -> f 0

    {- 
        Explicación del tipo: estamos aplicando 0 a cierta función por lo que f debe tomar ese 0, que es de tipo Integer, como argumento, 
                              entonces f :: (Integer -> t). A su vez, queremos ser capaces de aplicar eso a otro argumento, por lo que
                              (\f -> f 0) :: (Integer -> t) -> t


        Sí esta currificada, la podemos usar para crear otras funciones.
        Veamos algunos ejemplos
    -}

esMayorACero = evaluarEnCero (<) -- esto es lo mismo que hacer (<) 0 otroArgumento, que a su vez es lo mismo que 0 < otroArgumento
siempreSiete = evaluarEnCero (+) 7 -- hace (+) 0 7 = 0 + 7 = 7
estaElCero xs = evaluarEnCero elem xs -- se fija si 0 forma parte de una lista
ceroEsPar = evaluarEnCero even -- medio al dope pero es para ver que también lo podemos aplicar sobre funciones unarias

----

dosVeces :: (a -> a) -> a -> a
dosVeces = \f -> f . f

    {-
        Explicación del tipo:
            Primero veamos el tipo de la composición de funciones
                (.) :: (b -> c) -> (a -> b) -> a -> c
            Nosotros vamos a componer una función con sí misma por lo que el tipo de lo que devuelve debe ser igual a lo que recibe, entonces:
                f :: a -> a

        Está currificada ya que uno puede pasarle la función sin darle el elemento al cual quiero aplicarla para así poder crear más funciones.  
    -}

succSucc = dosVeces (\n -> n + 1)

----

-- map :: (a -> b) -> [a] -> [b]
-- flip :: (a -> b -> c) -> b -> a -> c

flipAll :: [a -> b -> c] -> [b -> a -> c] 
flipAll = map flip

flipRaro :: b -> (a -> b -> c) -> a -> c
flipRaro = flip flip

func1 = flipAll [(-), mod, div]

-- flipAllTest: map (\f -> f 2 10) func1
-- devuelve [8,0,5]
