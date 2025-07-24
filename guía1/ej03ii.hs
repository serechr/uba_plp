{-  
    Definir la función mejorSegún :: (a -> a -> Bool) -> [a] -> a, que devuelve el máximo elemento
    de la lista según una función de comparación, utilizando foldr1. Por ejemplo, maximum = mejorSegún (>). 
-}

mejorSegunRecExpl :: (a -> a -> Bool) -> [a] -> a
mejorSegunRecExpl _ [x] = x
mejorSegunRecExpl p (x:y:xs) = if p x y then mejorSegunRecExpl p (x:xs)
                                        else mejorSegunRecExpl p (y:xs)

-- foldr1: it takes the last two items of the list and applies the function, then it takes the third item from the end and the result, and so on.

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun p = foldr1 (\x y -> if p x y then x else y)