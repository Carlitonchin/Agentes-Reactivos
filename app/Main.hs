module Main where
import Tablero
import Elementos
import Escribir
import Listas
import Movimiento

tablero = iniciarTablero 4 4
tableroConNinho = escribir tablero (crearNinho 0 0)
tableroSucio = escribir tableroConNinho (crearSuciedad 1 1)
tableroRobot = escribir tableroSucio (crearRobot 1 3)
tableroCuna = escribir (escribir tableroRobot (crearCuna 2 3)) (crearNinho 2 2)
tableroObstaculo = escribir tableroCuna (crearObstaculo 1 0)
tableroObstaculo2 = escribir tableroObstaculo (crearObstaculo 2 0)
tableroObstaculo3 = escribir tableroObstaculo2 (crearObstaculo 3 0)

movidoNinho = mover tableroObstaculo3 (crearNinho 0 0) (1) (0)

main :: IO ()
main = putStrLn (show movidoNinho)

