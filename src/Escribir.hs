module Escribir (escribir) where

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
borrar tablero p  = tablero
----------- -------------------------------------------------------------------------

-----------------borrar------------------------------------------------------------


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