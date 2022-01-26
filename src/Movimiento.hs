module Movimiento where
import Tablero
import Elementos
import Escribir
import Listas

nuevaX:: Posicion p => Tablero -> p -> Int -> Int
nuevaX t p dx | nx == (ancho t) || nx == -1 = (x p)
                        | otherwise = nx
                        where nx = (x p) + dx 

nuevaY :: Posicion p => Tablero -> p -> Int -> Int
nuevaY t p dy | ny == (largo t) || ny == -1 = (y p)
              | otherwise = ny
              where ny = (y p) + dy

moverNinhoLibre :: Tablero -> Ninho -> Int -> Int -> Tablero
moverNinhoLibre t n nx ny | ninhoPuedeMoverse t nx ny = let borrarNinho = borrar t n in escribir borrarNinho (crearNinho nx ny)
                          | otherwise = t

moverObstaculoLibre :: Tablero -> Obstaculo -> Int -> Int -> Tablero
moverObstaculoLibre t o nx ny | estaVacio t nx ny = let borrado = borrar t o in escribir borrado (crearObstaculo nx ny)
                              | otherwise = t  

mover :: Posicion p => Tablero -> p -> Int -> Int -> Tablero
mover tablero p dx dy | tipo p == tipoNinho = moverNinho tablero (toNinho p) dx dy

moverNinho :: Tablero -> Ninho -> Int -> Int -> Tablero
moverNinho t n dx dy | hayObstaculo t nx ny = let obstaculoMovido = moverObstaculo t (crearObstaculo nx ny) dx dy in moverNinhoLibre obstaculoMovido n nx ny
                     | otherwise = moverNinhoLibre t n nx ny   
                     where nx = nuevaX t n dx
                           ny = nuevaY t n dy

moverObstaculo :: Tablero -> Obstaculo -> Int -> Int -> Tablero
moverObstaculo t o dx dy | nx == (x o) && ny == (y o) = t 
                         | hayObstaculo t nx ny = let obstaculoMovido = moverObstaculo t (crearObstaculo nx ny) dx dy in moverObstaculoLibre obstaculoMovido o nx ny
                         | otherwise = moverObstaculoLibre t o nx ny
                         where nx = nuevaX t o dx
                               ny = nuevaY t o dy 