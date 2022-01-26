module Escribir (escribir, borrar) where

import Elementos
import Tablero
import Listas

----- funciones a exportar -----------------------

escribir :: Posicion p => Tablero -> p -> Tablero
escribir tablero p | tipo p == tipoNinho = escribirNinho tablero (toNinho p)
                   | tipo p == tipoSuciedad = escribirSuciedad tablero (toSuciedad p)
                   | tipo p == tipoRobot = escribirRobot tablero (toRobot p) 
                   | tipo p == tipoCuna = escribirCuna tablero (toCuna p)
                   | otherwise = escribirObstaculo tablero (toObstaculo p)

borrar :: Posicion p => Tablero -> p -> Tablero
borrar tablero p  | tipo p == tipoNinho = borrarNinho tablero (toNinho p)
                  | tipo p == tipoSuciedad = borrarSuciedad tablero (toSuciedad p)
                  | tipo p == tipoRobot = borrarRobot tablero (toRobot p)
                  | tipo p == tipoCuna = borrarCuna tablero (toCuna p)
                  | otherwise = borrarObstaculo tablero (toObstaculo p)
----------- -------------------------------------------------------------------------

-----------------borrar------------------------------------------------------------
borrarNinho :: Tablero -> Ninho -> Tablero
borrarNinho t n = crearTablero (largo t) (ancho t) (suciedad t) (robots t) (cuna t) (eliminar n (ninhos t)) (obstaculos t)

borrarSuciedad :: Tablero -> Suciedad -> Tablero
borrarSuciedad t s = crearTablero (largo t) (ancho t) (eliminar s (suciedad t)) (robots t) (cuna t) (ninhos t) (obstaculos t)

borrarRobot :: Tablero -> Robot -> Tablero
borrarRobot t r = crearTablero (largo t) (ancho t) (suciedad t) (eliminar r (robots t)) (cuna t) (ninhos t) (obstaculos t)

borrarCuna :: Tablero -> Cuna -> Tablero
borrarCuna t c = crearTablero (largo t) (ancho t) (suciedad t) (robots t) (eliminar c (cuna t)) (ninhos t) (obstaculos t)

borrarObstaculo :: Tablero -> Obstaculo -> Tablero
borrarObstaculo t o = crearTablero (largo t) (ancho t) (suciedad t) (robots t) (cuna t) (ninhos t) (eliminar o (obstaculos t))
---------------- escribir--------------------------------------------

escribirNinho :: Tablero -> Ninho -> Tablero
escribirNinho t ninho = crearTablero (largo t) (ancho t) (suciedad t) (robots t) (cuna t) (agregar ninho (ninhos t)) (obstaculos t)

escribirSuciedad :: Tablero -> Suciedad -> Tablero
escribirSuciedad t suc = crearTablero (largo t) (ancho t) (agregar suc (suciedad t)) (robots t) (cuna t) (ninhos t) (obstaculos t)

escribirRobot :: Tablero -> Robot -> Tablero
escribirRobot t rob = crearTablero (largo t) (ancho t) (suciedad t) (agregar rob (robots t)) (cuna t) (ninhos t) (obstaculos t)

escribirCuna :: Tablero -> Cuna -> Tablero
escribirCuna t c = crearTablero (largo t) (ancho t) (suciedad t) (robots t) (agregar c (cuna t)) (ninhos t) (obstaculos t)

escribirObstaculo :: Tablero -> Obstaculo -> Tablero
escribirObstaculo t o = crearTablero (largo t) (ancho t) (suciedad t) (robots t) (cuna t) (ninhos t) (agregar o (obstaculos t))

-------------------------------------------------------------------------