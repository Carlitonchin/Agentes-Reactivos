module Main where
import Tablero
import Elementos
import Escribir
import Listas
import Movimiento
import Pintar
import Aleatorio
import Objetivos
import FuncionAgentes

-- sudo apt install fonts-emojione


tablero = iniciarTablero 4 4 1
tableroConNinho = escribir tablero (crearNinho 0 0)
tableroSucio = escribir tableroConNinho (crearSuciedad 1 1)
tableroRobot = escribir tableroSucio (crearRobot 1 3)
tableroCuna = escribir (escribir tableroRobot (crearCuna 2 3)) (crearNinho 2 2)
tableroObstaculo = escribir tableroCuna (crearObstaculo 1 0)
tableroObstaculo2 = escribir tableroObstaculo (crearObstaculo 2 0)

movidoNinho = moverDerecha tableroObstaculo2 (crearNinho 0 0) -- (1) (0)
movidoNinho2 = moverIzquierda movidoNinho (crearNinho 1 0) -- (-1) (0)
movidoRobot = moverArriba movidoNinho2 (crearRobot 1 3) --(0) (-1)
movidoRobot2 = moverArriba movidoRobot (crearRobot 1 2) --(0) (-1)
ninhoAbajo = moverAbajo movidoRobot2 (crearNinho 0 0) --(0) (1)
ninhoDerecha = moverIzquierda ninhoAbajo (crearRobot 1 1) --(-1) (0)
descargado = descargar ninhoDerecha 0 1
moverOtraVez = moverDerecha descargado (crearNinho 0 1) --(1) (0)
moverRobotOV = moverDerecha moverOtraVez (crearRobot 0 1) --(1) (0)
ninhoALaCuna = moverAbajo moverRobotOV (crearNinho 2 2) --(0) (1)
otroRobot = escribir ninhoALaCuna (crearRobot 2 3)
cargarAEseNinho = cargar otroRobot 2 3
--descargarAeseNinho = descargar cargarAEseNinho 2 3
moverConNinho = moverArriba cargarAEseNinho (crearRobot 2 3) --0 (-1)
--descargareseNinho = descargar moverConNinho 2 2
arriba = moverArriba moverConNinho (crearRobot 2 2 ) --0 (-1)
izquierda = moverIzquierda arriba (crearRobot 2 1) --(-1) (0)
abajo = moverAbajo izquierda (crearRobot 2 1) --(0) (1)
l1 = moverIzquierda abajo (crearRobot 2 2) --(-1) (0)
l2 = moverIzquierda l1 (crearRobot 1 2) --(-1) (0)
up = moverAbajo l2 (crearRobot 0 2) --(0) (1)
d1 = moverDerecha up (crearRobot 0 3) --(1) (0)
d2 = moverDerecha d1 (crearRobot 1 3) --1 0
dd2 = descargar d2 2 3
ra = moverArriba dd2 (crearRobot 2 3) --0 (-1)
nl = moverIzquierda ra (crearNinho 2 3) --(-1) 0
--limpio = limpiar nl (crearRobot 1 1)
robotIzq = moverIzquierda nl (crearRobot 1 1)
robotDown = moverAbajo robotIzq (crearRobot 0 1)
robotSinCargar = descargar robotDown 0 2


sem = 112321312112
tableroSemilla = iniciarTablero 4 4 sem
conCunas = generarCunas tableroSemilla 8
conNinhos = generarNinhos conCunas 8
conRobots = generarRobots tableroSemilla 2
conObstaculos = generarObstaculos conRobots 3
conSuciedad = generarSuciedades conRobots 10
casillasVacias = getEspaciosVacios tableroSemilla
trandom = random tableroSemilla
trandom2 = random trandom
nuevaPosicion = getPosicion trandom2 casillasVacias
nx = indexar nuevaPosicion 0
ny = indexar nuevaPosicion 1
array = [[1,1], [0,0]]
n1 = indexarLista array 1
tableroBfs = bfs conSuciedad
--o = Objetivo {robot = Robot {xRobot = 3, yRobot = 2, cargando = False}, costo = 4, tipoObjetivo = "limpiar", paso = (2,2), objx = 0, objy = 2}
objHastaAhora = crearObjetivo objVagancia (crearRobot 0 0) infinito (-1,-1) 0 0


main :: IO ()
main =  do
    putStrLn (iteraciones conSuciedad 20)
    
    
    --putStrLn (pintarTablero (iteracion conSuciedad))

iteraciones :: Tablero -> Int -> String
iteraciones t i 
    | i == 0 = pintarTablero t
    | otherwise = let nuevoTablero = iteracion t
                 in pintarTablero t ++ "\n\n\n" ++ (iteraciones nuevoTablero (i-1))
