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

-- instalar este software para imprimir los emojis en la consola
-- sudo apt install fonts-emojione


--CAmbiar estas variables para obtener una simulacion diferente
cantNinhos = 3
cantRobots = 8
cantSuciedad = 3
cantObstaculos = 1
anchoTablero = 7
largoTablero = 7
sem = 1
tAmbiente = 3
cantIteraciones = 30
---------------------------------------------------------------------------


tableroSemilla = iniciarTablero largoTablero anchoTablero sem
conNinhos = generarNinhos tableroSemilla cantNinhos
conRobots = generarRobots conNinhos cantRobots
conCuna = generarCunas conRobots cantNinhos
conObstaculos = generarObstaculos conCuna cantObstaculos
conSuciedad = generarSuciedades conObstaculos cantSuciedad

simulacion = iteracionesTablero conSuciedad 1 [conSuciedad]
simulacionPintura = pintarArrayTablero simulacion 0

main :: IO ()
main =  do
    putStrLn simulacionPintura



pintarArrayTablero :: [Tablero] -> Int -> String
pintarArrayTablero tableros index
    | index == length tableros = ""
    | otherwise = (show (index+1)) ++ turnoAmb ++ (pintarTablero tableroIndex) ++ pintarArrayTablero tableros (index + 1)
    where
        turnoAmb = if mod index tAmbiente == 0 && index /= 0
                   then " (Turno del Ambiente)\n"
                   else "\n"
        tableroIndex = indexar tableros index 

iteracionesTablero :: Tablero -> Int -> [Tablero] -> [Tablero]
iteracionesTablero t i acumulado
    | i == (cantIteraciones) = acumulado
    | otherwise = let nt = iteracion t
                      
                      turnoAmb = if (mod i tAmbiente) == 0
                                 then turnoAmbiente nt
                                 else nt
                      nuevoAcumulado = agregar turnoAmb acumulado
                   in iteracionesTablero turnoAmb (i+1) nuevoAcumulado 
                 
