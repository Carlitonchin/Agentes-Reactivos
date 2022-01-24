module Main where
import Tablero
import Elementos
import Escribir

tablero = iniciarTablero 4 4
tableroConNinho = escribir tablero (crearNinho 0 0)
tableroSucio = escribir tableroConNinho (crearSuciedad 1 1)
tableroRobot = escribir tableroSucio (crearRobot 5 1)
tableroCuna = escribir (escribir tableroRobot (crearCuna 2 3)) (crearNinho 2 2)
tableroObstaculo = escribir tableroCuna (crearObstaculo 4 2)

main :: IO ()
main = putStrLn (show tableroObstaculo)

