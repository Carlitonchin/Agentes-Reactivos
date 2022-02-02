module Objetivos where

import Tablero
import Elementos
import Listas
import Movimiento

bfs :: Tablero -> Tablero
bfs t = bfsLista t (robots t)

getPrimerPaso :: Tablero -> Int -> Int -> Robot -> (Int, Int)
getPrimerPaso t px py r = bfsPrimerPaso t r [(px, py)] (-1,-1) [[px, py, -1]]  

bfsPrimerPaso :: Tablero -> Robot -> [(Int, Int)] -> (Int, Int) -> [[Int]] -> (Int, Int)
bfsPrimerPaso t r visitadas padre (e:resto) 
    | ex == rx && ey == ry = padre
    | otherwise = bfsPrimerPaso t r nuevasVisitadas (ex, ey) nuevaCola
    where 
        ex = indexar e 0
        ey = indexar e 1
        rx = x r 
        ry = y r
        nuevasVisitadas = agregar (ex, ey) visitadas
        nuevaCola = iteracionBfs t ex ey r nuevasVisitadas resto (-1)

iteracionBfs :: Tablero -> Int -> Int -> Robot -> [(Int, Int)] -> [[Int]] -> Int -> [[Int]]
iteracionBfs t px py r visitados cola cont
    | estaCargado t xr yr = anhadeALaCola t [(arribaX, arribaY), (abajoX, abajoY), (izqX, izqY), (derX, derY)] cola visitados cont
    | otherwise = anhadeALaCola t [(arribaX, arribaY), (abajoX, abajoY), (izqX, izqY), (derX, derY)] cola visitados cont
    where 
        xr = x r
        yr = y r
        robotImaginario = crearRobot px py
        arribaX = px
        arribaY = nuevaY t robotImaginario (-1)
        abajoX = px
        abajoY = nuevaY t robotImaginario 1
        izqX = nuevaX t robotImaginario (-1)
        izqY = py
        derX = nuevaX t robotImaginario 1
        derY = py
        --faltan los movimientos de 2 pasos

anhadeALaCola :: Tablero -> [(Int, Int)] -> [[Int]] -> [(Int, Int)] -> Int -> [[Int]]
anhadeALaCola t [] cola visitados cont = cola 
anhadeALaCola t (f:r) cola visitadas cont = 
    let nuevaCola = if (not (pertenece f visitadas)) && ((estaVacio t xf yf) || (haySuciedad t xf yf) || (hayCuna t xf yf))  then agregar [xf, yf, cont + 1] cola else cola 
    in anhadeALaCola t r nuevaCola visitadas cont
    where 
        xf = fst f
        yf = snd f

bfsLista :: Tablero -> [Robot] -> Tablero
bfsLista t [] = t
bfsLista t (r1:rr) = bfsLista nuevoTablero rr
                    where
                        casillasMarcadas = []
                        cola = [[x r1, y r1, 0]]
                        nuevoTablero = bfsElemento t r1 casillasMarcadas cola


bfsElemento :: Tablero -> Robot -> [(Int,Int)] -> [[Int]]-> Tablero
bfsElemento t r marcadas [] = t
bfsElemento t r marcadas (pos:cola) = bfsElemento nuevoTablero r nuevasMarcadas nuevaCola
        where
        xr = x r
        yr = y r
        px = indexar pos 0
        py = indexar pos 1
        cont = indexar pos 2
        nuevoTablero = agregarObjetivo t r px py cont
        nuevasMarcadas = agregar (xr, yr) marcadas
        nuevaCola = iteracionBfs t px py r nuevasMarcadas cola cont

agregarObjetivo :: Tablero -> Robot -> Int -> Int -> Int-> Tablero
agregarObjetivo t r px py costo
    | tipoElemento == tipoSuciedad = agregarObjetivoSuciedad t r px py costo
    | otherwise = t
    where
        tipoElemento = get t px py

agregarObjetivoSuciedad :: Tablero -> Robot -> Int -> Int -> Int -> Tablero
agregarObjetivoSuciedad t r px py cost =
    crearTablero largoV anchoV suciedadV robotsV cunasV ninhosV obstaculosV cargadosV (agregar nuevoObjetivo objetivosV) semillaV
    where
        robotsV = robots t
        ninhosV = ninhos t
        suciedadV = suciedad t
        obstaculosV = obstaculos t
        cunasV = cuna t
        semillaV = semilla t
        cargadosV = cargados t
        largoV = largo t
        anchoV = ancho t
        objetivosV = objetivos t
        nuevoObjetivo = crearObjetivoSuciedad t r px py cost


crearObjetivoSuciedad :: Tablero -> Robot -> Int -> Int -> Int -> Objetivo
crearObjetivoSuciedad t r px py cost 
    | estaCargado t rx ry = error "notIMplementException"
    | otherwise = crearObjetivo objLimpiar r (cost + 1) paso1
    where 
        rx = x r
        ry = y r 
        paso1 = getPrimerPaso t px py r