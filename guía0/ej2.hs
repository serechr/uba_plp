-- Ejercicio 2

valorAbsoluto1 :: Float -> Float
valorAbsoluto1 n | n < 0 = -n
                 | otherwise = n

valorAbsoluto2 :: Float -> Float
valorAbsoluto2 n = if n < 0 then -n else n

bisiesto :: Int -> Bool
bisiesto n = (mod n 4 == 0 && mod n 100 /= 0) || mod n 400 == 0

factorial1 :: Int -> Int
factorial1 n | n == 0 = 1 
             | otherwise = n * factorial (n - 1)

factorial2 :: Int -> Int
factorial2 n = if n == 0 then 1 else n * factorial (n - 1)

divisores :: Int -> [Int]
divisores n = filter (\f -> mod n f == 0) [1..n]

esPrimo :: Int -> Bool
esPrimo n = length (divisores n) == 2

primos :: [Int] -> [Int]
primos xs = filter (\x -> esPrimo(x)) xs

cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos n = length (primos (divisores n))