import Data.List -- para poder usar nub

data Prop =  Var String | No Prop | Y Prop Prop | O Prop Prop | Imp Prop Prop
type Valuación = String -> Bool

-- Dar el tipo y definir las funciones foldProp y recProp
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

{- Definir la función variables :: Prop -> [String], que dada una fórmula 
devuelve la lista con todas sus variables proposicionales en algún orden, 
sin elementos repetidos. -}
variables :: Prop -> [String]
variables p = nub (foldProp (:[]) id (++) (++) (++) p)

-- Definir la función evaluar :: Valuación -> Prop -> Bool, que indica si una fórmula es verdadera o falsa para una valuación dada.
evaluar :: Valuación -> Prop -> Bool
evaluar v = foldProp v not (&&) (||) (\p1 p2 -> not p1 || p2)

{- Definir la función estáEnFNN :: Prop -> Bool, que indica si una fórmula está en Forma Normal Negada. 
Es decir, si no tiene implicaciones y la negación se aplica únicamente a variables y no a proposiciones más complejas. -}
estáEnFNN :: Prop -> Bool
estáEnFNN = recProp (\_ -> True)
                    (\p recp -> esVar p && recp)    
                    (\_ _ recp1 recp2 -> recp1 && recp2) 
                    (\_ _ recp1 recp2 -> recp1 && recp2) 
                    (\_ _ _ _ -> False)

esVar :: Prop -> Bool
esVar (Var _) = True
esVar _ = False