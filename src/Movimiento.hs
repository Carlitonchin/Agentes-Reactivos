module Movimiento where
import Tablero
import Elementos
import Escribir
import Listas

nuevaX:: Posicion p => Tablero -> p -> Int -> Int
nuevaX t p dx | nx >= (ancho t) || nx <= -1 = (x p)
                        | otherwise = nx
                        where nx = (x p) + dx 

nuevaY :: Posicion p => Tablero -> p -> Int -> Int
nuevaY t p dy | ny >= (largo t) || ny <= -1 = (y p)
              | otherwise = ny
              where ny = (y p) + dy

moverNinhoLibre :: Tablero -> Ninho -> Int -> Int -> Tablero
moverNinhoLibre t n nx ny | ninhoPuedeMoverse t n nx ny = let borrarNinho = borrar t n in escribir borrarNinho (crearNinho nx ny)
                          | otherwise = t

moverRobotLibre :: Tablero -> Robot -> Int -> Int -> Tablero
moverRobotLibre t r dx dy | estaCargado t (xr) (yr) = moverNinhoCargado t r dx dy
                          | otherwise = escribir robotBorrado (crearRobot nx ny) 
                          where xr = x r 
                                yr = y r 
                                nx = nuevaX t r dx
                                ny = nuevaY t r dy
                                robotBorrado = borrar t r 

moverNinhoCargado :: Tablero -> Robot -> Int -> Int -> Tablero
moverNinhoCargado t r dx dy = let xr = x r 
                                  yr = y r 
                                  nx = nuevaX t r dx
                                  ny = nuevaY t r dy
                              in 
                                    (let  descargado = descargar t xr yr
                                    in 
                                          (let borrado = borrar descargado (crearNinho xr yr)
                                          in 
                                               let movido = escribir borrado (crearNinho nx ny) 
                                               in mover movido r dx dy))



moverObstaculoLibre :: Tablero -> Obstaculo -> Int -> Int -> Tablero
moverObstaculoLibre t o nx ny | estaVacio t nx ny = let borrado = borrar t o in escribir borrado (crearObstaculo nx ny)
                              | otherwise = t  

moverArriba t p = mover t p 0 (-1)
moverAbajo t p = mover t p 0 1
moverDerecha t p = mover t p 1 0
moverIzquierda t p = mover t p (-1) 0

mover2Arriba t p = mover t p 0 (-2)
mover2Abajo t p = mover t p 0 2
mover2Derecha t p = mover t p 2 0
mover2Izquierda t p = mover t p (-2) 0

limpiar :: Tablero -> Robot -> Tablero
limpiar t r = borrar t sucio
             where sucio = crearSuciedad (x r) (y r)

mover :: Posicion p => Tablero -> p -> Int -> Int -> Tablero
mover tablero p dx dy | tipo p == tipoNinho = moverNinho tablero (toNinho p) dx dy
                      | tipo p == tipoRobot = moverRobot tablero (toRobot p) dx dy  

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

moverRobot :: Tablero -> Robot -> Int -> Int -> Tablero
moverRobot t r dx dy 
                     | not(robotPuedeMoverse t r nx ny) = t
                     | abs dx > 1 || abs dy > 1 =  let tm2 = mover t r ndx ndy
                                                   in mover tm2 r2 ndx ndy    
                     | hayNinho t nx ny = let tableroCargue = cargar nuevoT nx ny in moverRobotLibre tableroCargue r dx dy
                     | otherwise = moverRobotLibre nuevoT r dx dy
                     where  xr = x r
                            yr = y r
                            ndx = if dx == 0 then 0 else (div dx (abs (dx)))
                            ndy = if dy == 0 then 0 else (div dy (abs (dy)))
                            n1x = xr + ndx
                            n1y = yr + ndy
                            r2 = crearRobot n1x n1y
                            nx = nuevaX t r dx
                            ny = nuevaY t r dy
                            nuevoT = borrar t (crearRobot (x r) (y r))    