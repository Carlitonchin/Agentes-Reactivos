module Listas (agregar, eliminar, pertenece) where

agregar x lista = lista ++ [x]

eliminar x [] = []
eliminar x (f:r) | x == f = r
                 | otherwise = [f] ++ (eliminar x r) 

pertenece x [] = False
pertenece x (f:r) = x == f || pertenece x r