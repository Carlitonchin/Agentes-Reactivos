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
import MovimientoNinhos

-- sudo apt install fonts-emojione


sem = 109009211157976
tableroSemilla = iniciarTablero 6 6 sem
conNinhos = generarNinhos tableroSemilla 3
conRobots = generarRobots conNinhos 2
conCuna = generarCunas conRobots 3
conObstaculos = generarObstaculos conCuna 3
conSuciedad = generarSuciedades conObstaculos 0


cant = 30

pintura = iteracionesPintar conSuciedad cant
tablero = iteracionesTablero conSuciedad cant
--obs = escribir tablero (crearObstaculo 2 3)
ma = robotPuedeMoverse tablero (crearRobot 4 0) 4 2
--md = mover2Derecha ma (crearRobot 1 2)
t = getCuadricula tablero (crearNinho 0 0)
nt = getNinhosCuadricula tablero t []
qn = moverNinhosDeCuadricula tablero nt
main :: IO ()
main =  do
    putStrLn pintura
    -- print t
    -- print nt
    -- putStrLn (pintarTablero qn)
    --print "\n"
    --  print (robotPuedeMoverse tablero (crearRobot 3 0) 0 (-2))
    --putStrLn (pintarTablero tablero)
    --  print "\n\n"
    --print ma
    --  print "\n\n"
    --  putStrLn (pintarTablero md)
    -- putStrLn (pintarTablero md)
    -- putStrLn (pintarTablero md2)
    
    --print (objetivos tablero)
    
    
    
    --putStrLn (pintarTablero (iteracion conSuciedad))

iteracionesPintar :: Tablero -> Int -> String
iteracionesPintar t i 
    | i == 0 = (show (objetivos t)) ++ "\n" ++ pintarTablero t
    | otherwise = let nuevoTablero = iteracion t
                      movNinhos = turnoAmbiente nuevoTablero
                 in (show (objetivos t)) ++ "\n" ++ pintarTablero t ++ (iteracionesPintar movNinhos (i-1))


iteracionesTablero :: Tablero -> Int -> Tablero
iteracionesTablero t i 
    | i == 0 = t
    | otherwise = let nt = iteracion t
                   in iteracionesTablero nt (i-1)  
                 
