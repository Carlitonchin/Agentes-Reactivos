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
import GHC.Float

-- instalar este software para imprimir los emojis en la consola
-- sudo apt install fonts-emojione


--CAmbiar estas variables para obtener una simulacion diferente
cantNinhos =30
cantRobots = 11
cantSuciedad = 3
cantObstaculos = 5
anchoTablero = 20
largoTablero = 20
sem = 1
tAmbiente = 1
cantIteraciones = 300
---------------------------------------------------------------------------

emoji = True

tableroSemilla = iniciarTablero largoTablero anchoTablero sem
conNinhos = generarNinhos tableroSemilla cantNinhos
conRobots = generarRobots conNinhos cantRobots
conCuna = generarCunas conRobots cantNinhos
conObstaculos = generarObstaculos conCuna cantObstaculos
conSuciedad = generarSuciedades conObstaculos cantSuciedad

simulacion1 = iteracionesTablero conSuciedad 1 [conSuciedad]
simulacionPintura = pintarArrayTablero simulacion1 0 0

main :: IO ()
main =  do
    putStrLn simulacionPintura
    --print (simulacionCompleta 1 [])
    --print (calcularPeorPorcentaje simulacion1 0 0)

simulacionCompleta :: Int -> [Float] -> Float
simulacionCompleta semi acumulado
    | semi == 2 = promedio acumulado 0 0
    | otherwise = 
        let
            tableroSemilla = iniciarTablero largoTablero anchoTablero semi
            conNinhos = generarNinhos tableroSemilla cantNinhos
            conRobots = generarRobots conNinhos cantRobots
            conCuna = generarCunas conRobots cantNinhos
            conObstaculos = generarObstaculos conCuna cantObstaculos
            conSuciedad = generarSuciedades conObstaculos cantSuciedad
            simulacion = iteracionesTablero conSuciedad 1 [conSuciedad]
            ps = calcularPeorPorcentaje simulacion 0 0
            nuevoAcumulado = agregar ps acumulado
        in simulacionCompleta (semi + 1) nuevoAcumulado


promedio :: [Float] -> Float -> Int -> Float
promedio list sum index =
    if index == (((length list)) )
    then sum/(int2Float (length list))
    else
        let
             nuevoSum = sum + (indexar list index)
        in    promedio list nuevoSum (index + 1)
        
        

porcentajeDeSuciedad :: Tablero -> Float
porcentajeDeSuciedad t =
    let
        cantVacios = int2Float (length (getEspaciosVacios t))
        cantChurre = int2Float (length (suciedad t))
        total = cantChurre + cantVacios
    in
        cantChurre/total*100

pintarArrayTablero :: [Tablero] -> Int -> Float -> String
pintarArrayTablero tableros index peorPorcentaje
    | index == length tableros = "Peor porcentaje = " ++ (show peorPorcentaje)
    | otherwise = (show index) ++ turnoAmb ++ (pintarTablero tableroIndex emoji) ++ pintarArrayTablero tableros (index + 1) peorPorcentajeNuevo
    where
        turnoAmb = if mod index tAmbiente == 0 && index /= 0
                   then " " ++ (show ps) ++ " %s (Turno del Ambiente)\n"
                   else " " ++ (show ps) ++ " %s\n"
        tableroIndex = indexar tableros index 
        ps = porcentajeDeSuciedad tableroIndex
        peorPorcentajeNuevo = max ps peorPorcentaje

calcularPeorPorcentaje :: [Tablero] -> Int -> Float -> Float
calcularPeorPorcentaje tableros index peorPorcentaje
    | index == length tableros = peorPorcentaje
    | otherwise = calcularPeorPorcentaje tableros (index + 1) nuevoPeorPorcentaje
    where
        tn = indexar tableros index
        ps = porcentajeDeSuciedad tn
        nuevoPeorPorcentaje = max peorPorcentaje ps

iteracionesTablero :: Tablero -> Int -> [Tablero] -> [Tablero]
iteracionesTablero t i acumulado
    | i == (cantIteraciones + 1) = acumulado
    | otherwise = let nt = iteracion t
                      
                      turnoAmb = if (mod i tAmbiente) == 0
                                 then turnoAmbiente nt
                                 else nt
                      nuevoAcumulado = agregar turnoAmb acumulado
                   in iteracionesTablero turnoAmb (i+1) nuevoAcumulado 
                 
