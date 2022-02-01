module Listas where

agregar x lista = lista ++ [x]

eliminar :: Eq a => a -> [a] -> [a]
eliminar x [] = []
eliminar x (f:r) | x == f = r
                 | otherwise = [f] ++ (eliminar x r) 

pertenece x [] = False
pertenece x (f:r) = x == f || pertenece x r

quitarTodos :: Eq a => [a] -> [a] -> [a]
quitarTodos [] fuente = fuente
quitarTodos (f:r) fuente = quitarTodos r (eliminar f fuente)

indexar :: [Int] -> Int -> Int
indexar [] i = -1
indexar (f:r) i | i == 0 = f
                | otherwise = indexar r (i-1)

indexarLista :: [[a]] -> Int -> [a]
indexarLista [] i = []
indexarLista (f:r) i | i == 0 = f 
                     | otherwise = indexarLista r (i-1)