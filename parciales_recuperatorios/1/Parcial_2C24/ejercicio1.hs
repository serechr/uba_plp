import Data.List

data Buffer a = Empty 
              | Write Int a (Buffer a) 
              | Read Int (Buffer a)

foldBuffer :: b -> (Int -> a -> b -> b) -> (Int -> b -> b) -> Buffer a -> b 
foldBuffer fEmpty fWrite fRead buf = case buf of
                                    Empty -> fEmpty
                                    Write n c rBuf -> fWrite n c (rec rBuf)
                                    Read n rBuf -> fRead n (rec rBuf)
        where
            rec = foldBuffer fEmpty fWrite fRead

recBuffer :: b -> (Int -> a -> Buffer a -> b -> b) -> (Int -> Buffer a -> b -> b) -> Buffer a -> b 
recBuffer fEmpty fWrite fRead buf = case buf of
                                    Empty -> fEmpty
                                    Write n c rBuf -> fWrite n c rBuf (rec rBuf) 
                                    Read n rBuf -> fRead n rBuf (rec rBuf)
        where
            rec = recBuffer fEmpty fWrite fRead

posicionesOcupadas :: Buffer a -> [Int]
posicionesOcupadas b = nub (foldBuffer [] (\n _ rec -> n : rec) (\n rec -> filter (/= n) rec) b)
-- otra opción que ví en cubawiki es usar union para el caso Write y ahorrarse el nub fuera del foldBuffer

contenido:: Int -> Buffer a -> Maybe a
contenido n = foldBuffer Nothing
                         (\m c rec -> if n == m then Just c else Nothing)
                         (\m rec   -> if n == m then Nothing else rec)

puedeCompletarLecturas :: Buffer a -> Bool
puedeCompletarLecturas = recBuffer True
                                   (\_ _ _ rBuf -> rBuf)
                                   (\n b rBuf -> elem n (posicionesOcupadas b) && rBuf)

deshacer :: Buffer a -> Int -> Buffer a
deshacer = recBuffer (const Empty)
                     (\n c b rBuf m -> if m == 0 then (Write n c b) else rBuf (m-1))
                     (\n b rBuf m -> if m == 0 then (Read n b) else rBuf (m-1))


buf = Write 1 "a" $ Write 2 "b" $ Write 1 "c" $ Empty