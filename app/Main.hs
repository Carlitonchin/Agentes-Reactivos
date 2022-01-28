module Main where
import Tablero
import Elementos
import Escribir
import Listas
import Movimiento
import Pintar

-- sudo apt install fonts-emojione

tablero = iniciarTablero 4 4
tableroConNinho = escribir tablero (crearNinho 0 0)
tableroSucio = escribir tableroConNinho (crearSuciedad 1 1)
tableroRobot = escribir tableroSucio (crearRobot 1 3)
tableroCuna = escribir (escribir tableroRobot (crearCuna 2 3)) (crearNinho 2 2)
tableroObstaculo = escribir tableroCuna (crearObstaculo 1 0)
tableroObstaculo2 = escribir tableroObstaculo (crearObstaculo 2 0)

movidoNinho = mover tableroObstaculo2 (crearNinho 0 0) (1) (0)
movidoNinho2 = mover movidoNinho (crearNinho 1 0) (-1) (0)
movidoRobot = mover movidoNinho2 (crearRobot 1 3) (0) (-1)
movidoRobot2 = mover movidoRobot (crearRobot 1 2) (0) (-1)
ninhoAbajo = mover movidoRobot2 (crearNinho 0 0) (0) (1)
ninhoDerecha = mover ninhoAbajo (crearRobot 1 1) (-1) (0)
descargado = descargar ninhoDerecha 0 1
moverOtraVez = mover descargado (crearNinho 0 1) (1) (0)
moverRobotOV = mover moverOtraVez (crearRobot 0 1) (1) (0)
ninhoALaCuna = mover moverRobotOV (crearNinho 2 2) (0) (1)
otroRobot = escribir ninhoALaCuna (crearRobot 2 3)
cargarAEseNinho = cargar otroRobot 2 3
--descargarAeseNinho = descargar cargarAEseNinho 2 3
moverConNinho = mover cargarAEseNinho (crearRobot 2 3) 0 (-1)
--descargareseNinho = descargar moverConNinho 2 2
arriba = mover moverConNinho (crearRobot 2 2 ) 0 (-1)
izquierda = mover arriba (crearRobot 2 1) (-1) (0)

main :: IO ()
main = putStrLn (pintarTablero izquierda)

