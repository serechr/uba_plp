-- Ejercicio 3

data Maybe a = Nothing | Just a

inverso :: Float -> Maybe Float
inverso 0 = Nothing
inverso n = Just (1/n) 

data Either a b = Left a | Right b

aEntero :: Either Int Bool -> Int
aEntero (Left _) = -1
aEntero (Right True) = 1
aEntero (Right False) = 0