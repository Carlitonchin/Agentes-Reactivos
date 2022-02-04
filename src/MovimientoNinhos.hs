module MovimientoNinhos where

import Aleatorio
import Tablero
import Elementos
import Listas
import Movimiento

turnoAmbiente :: Tablero -> Tablero
turnoAmbiente t = moverNinhos t (ninhos t)

moverNinhos :: Tablero -> [Ninho] -> Tablero
moverNinhos t [] = t
moverNinhos t (n1:rn) =
    let
        cuadricula = getCuadricula t n1
        ninhosCuadricula = getNinhosCuadricula t cuadricula []
        nuevoTablero = moverNinhosDeCuadricula t ninhosCuadricula
        restoNinhos = quitarTodos ninhosCuadricula rn
    in moverNinhos nuevoTablero restoNinhos

moverNinhosDeCuadricula :: Tablero -> [Ninho] -> Tablero
moverNinhosDeCuadricula t [] = t
moverNinhosDeCuadricula t (n1:rn) =
        let nTablero = moverEsteNinho t n1
        in moverNinhosDeCuadricula nTablero rn
    

indexarMovimiento [] i = moverArriba
indexarMovimiento (m1:rm) i
    | i == 0 = m1
    | otherwise = indexarMovimiento rm (i-1)

moverEsteNinho :: Tablero -> Ninho -> Tablero
moverEsteNinho t n = 
    let 
        tRandom = random t
        movs = [moverArriba, moverDerecha, moverAbajo, moverIzquierda]
        rNum = randomNum (semilla tRandom) (length movs)
        direccion = indexarMovimiento movs rNum
    in direccion t n

getNinhosCuadricula:: Tablero -> [(Int, Int)] -> [Ninho]-> [Ninho]
getNinhosCuadricula t [] acumulado = acumulado
getNinhosCuadricula t (c1:rc) acumulado =
    let
        nuevoAcumulado = if hayNinho t cx cy
                         then agregar (crearNinho cx cy) acumulado 
                         else acumulado
    in getNinhosCuadricula t rc nuevoAcumulado
    where
        cx = fst c1
        cy = snd c1

getCuadricula :: Tablero -> Ninho -> [(Int, Int)]
getCuadricula t n    
    | esPosible t centro = getCuadriculaPorCentro centro
    | esPosible t centroDer = getCuadriculaPorCentro centroDer
    | esPosible t centroIzq = getCuadriculaPorCentro centroIzq
    | esPosible t centroArriba = getCuadriculaPorCentro centroArriba
    | esPosible t centroAbajo = getCuadriculaPorCentro centroAbajo
    | esPosible t centroDAD = getCuadriculaPorCentro centroDAD
    | esPosible t centroDAI = getCuadriculaPorCentro centroDAI
    | esPosible t centroDBD = getCuadriculaPorCentro centroDBD
    | esPosible t centroDBI = getCuadriculaPorCentro centroDBI
    | otherwise = error "No se pudo conseguir ninguna cuadricula valida"
    where
        xn = x n
        yn = y n
        centro = (xn, yn)
        centroDer = (xn+1, yn)
        centroIzq = (xn-1, yn)
        centroArriba = (xn, yn+1)
        centroAbajo = (xn, yn - 1)
        centroDAD = (xn+1, yn+1)
        centroDAI = (xn-1, yn+1)
        centroDBD = (xn+1, yn-1)
        centroDBI = (xn-1, yn-1)

esPosible :: Tablero -> (Int, Int) -> Bool
esPosible t centro 
    | seSaleDelTablero t cx cy = False
    | seSaleDelTablero t (cx+1) cy = False
    | seSaleDelTablero t (cx-1) cy = False
    | seSaleDelTablero t (cx) (cy+1) = False
    | seSaleDelTablero t (cx) (cy-1) = False
    | seSaleDelTablero t (cx+1) (cy+1) = False
    | seSaleDelTablero t (cx+1) (cy-1) = False
    | seSaleDelTablero t (cx-1) (cy-1) = False
    | seSaleDelTablero t (cx-1) (cy+1) = False
    | otherwise = True
    where
        cx = fst centro
        cy = snd centro

seSaleDelTablero :: Tablero -> Int -> Int -> Bool
seSaleDelTablero t px py = px < 0 || px >= a || py < 0 || py >= l
    where
        a = ancho t
        l = largo t

getCuadriculaPorCentro :: (Int, Int) -> [(Int,Int)]
getCuadriculaPorCentro centro = [(cx, cy), (cx+1, cy), (cx-1, cy), (cx, cy+1), (cx, cy-1), (cx+1, cy+1), (cx-1, cy-1), (cx-1, cy+1), (cx+1, cy-1)]
    where
        cx = fst centro
        cy = snd centro