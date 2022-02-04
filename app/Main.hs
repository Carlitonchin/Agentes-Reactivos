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


sem = 1123141221323
tableroSemilla = iniciarTablero 8 5 sem
conNinhos = generarNinhos tableroSemilla 5
conRobots = generarRobots conNinhos 2
conCuna = generarCunas conRobots 5
conObstaculos = generarObstaculos conCuna 10
conSuciedad = generarSuciedades conObstaculos 3

cant = 25

pintura = iteracionesPintar conSuciedad cant
tablero = iteracionesTablero conSuciedad cant
--obs = escribir tablero (crearObstaculo 2 3)
ma = robotPuedeMoverse tablero (crearRobot 4 0) 4 2
--md = mover2Derecha ma (crearRobot 1 2)

main :: IO ()
main =  do
    putStrLn pintura
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
    | i == 0 = (show (objetivos t)) ++ "\n" ++ pintarTablero t ++ "\n" 
    | otherwise = let nuevoTablero = iteracion t
                 in (show (objetivos t)) ++ "\n" ++ pintarTablero t  ++ "\n\n\n" ++ (iteracionesPintar nuevoTablero (i-1))


iteracionesTablero :: Tablero -> Int -> Tablero
iteracionesTablero t i 
    | i == 0 = t
    | otherwise = let nt = iteracion t
                   in iteracionesTablero nt (i-1)  
                 
