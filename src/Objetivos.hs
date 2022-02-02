module Objetivos where

import Tablero
import Elementos
import Listas
import Movimiento
import Pintar

infinito = 999999999

bfs :: Tablero -> Tablero
bfs t = bfsLista t (robots t)

limpiarObjetivos :: Tablero -> [Objetivo] -> Tablero
limpiarObjetivos t objE = crearTablero (largo t) (ancho t) (suciedad t) (robots t) (cuna t) (ninhos t) (obstaculos t) (cargados t) (quitarTodos objE (objetivos t)) (semilla t)

filtrar :: [Objetivo] -> Robot -> [Objetivo] -> [Objetivo]
filtrar [] r acumulado = acumulado
filtrar (o1:or) r acumulado = filtrar or r nuevoAcumulado
    where
        ro = robot o1
        nuevoAcumulado = if ro == r
                        then agregar o1 acumulado
                        else acumulado

getPrimerPaso :: Tablero -> Int -> Int -> Robot -> (Int, Int)
getPrimerPaso t px py r = bfsPrimerPaso t r [(px, py)] (-1,-1) [[px, py, -1]]  

bfsPrimerPaso :: Tablero -> Robot -> [(Int, Int)] -> (Int, Int) -> [[Int]] -> (Int, Int)
bfsPrimerPaso t r visitadas padre [] = padre
bfsPrimerPaso t r visitadas padre (e:resto) 
    | aDistancia1DeRobot t r ex ey = (ex, ey)
    | otherwise = bfsPrimerPaso t r nuevasVisitadas (ex, ey) nuevaCola
    where 
        ex = indexar e 0
        ey = indexar e 1
        rx = x r 
        ry = y r
        nuevaColaVisitadas = iteracionBfs t ex ey r visitadas resto (-1)
        nuevaCola = fst nuevaColaVisitadas
        nuevasVisitadas = snd nuevaColaVisitadas

aDistancia1DeRobot :: Tablero -> Robot -> Int -> Int -> Bool
aDistancia1DeRobot t r px py
    | xr == arribaX && yr == arribaY = True
    | xr == abajoX && yr == abajoY = True
    | xr == derX && yr == derY = True
    | xr == izqX && yr == izqY = True
    | otherwise = False
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


iteracionBfs :: Tablero -> Int -> Int -> Robot -> [(Int, Int)] -> [[Int]] -> Int -> ([[Int]],[(Int,Int)])
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

anhadeALaCola :: Tablero -> [(Int, Int)] -> [[Int]] -> [(Int, Int)] -> Int -> ([[Int]],[(Int,Int)])
anhadeALaCola t [] cola visitados cont = (cola,visitados)
anhadeALaCola t (f:r) cola visitados cont = anhadeALaCola t r nuevaCola nuevoVisitados cont
    where 
        xf = fst f
        yf = snd f
        nuevaColaVisitados = if (not (pertenece f visitados)) && ((estaVacio t xf yf) || (haySuciedad t xf yf) || (hayCuna t xf yf))  
        then (agregar [xf, yf, cont + 1] cola, agregar (xf,yf) visitados)
        else (cola,visitados)
        nuevaCola = fst nuevaColaVisitados
        nuevoVisitados = snd nuevaColaVisitados

bfsLista :: Tablero -> [Robot] -> Tablero
bfsLista t [] = t
bfsLista t (r1:rr) 
    | (habemusSuciedad t (x r1) (y r1)) 
        = let nTablero = agregarObjetivoLimpiarSuciedad t r1
          in bfsLista nTablero rr  
    | otherwise = bfsLista nuevoTablero rr
                    where
                        casillasMarcadas = [(x r1, y r1)]
                        cola = [[x r1, y r1, 0]]
                        nuevoTablero = bfsElemento t r1 casillasMarcadas cola


bfsElemento :: Tablero -> Robot -> [(Int,Int)] -> [[Int]]-> Tablero
bfsElemento t r marcadas [] = t
bfsElemento t r marcadas (pos:cola) = 
    if nuevoTablero == t
    then bfsElemento nuevoTablero r nuevasMarcadas nuevaCola
    else nuevoTablero
        where
        xr = x r
        yr = y r
        px = indexar pos 0
        py = indexar pos 1
        cont = indexar pos 2
        nuevoTablero = agregarObjetivo t r px py cont
        nuevaColaVisitadas = iteracionBfs t px py r marcadas cola cont
        nuevaCola = fst nuevaColaVisitadas
        nuevasMarcadas = snd nuevaColaVisitadas

agregarObjetivo :: Tablero -> Robot -> Int -> Int -> Int-> Tablero
agregarObjetivo t r px py costo
    | tipoElemento == tipoSuciedad = agregarObjetivoSuciedad t r px py costo
    | otherwise = t
    where
        tipoElemento = get t px py

agregarObjetivoLimpiarSuciedad :: Tablero -> Robot -> Tablero
agregarObjetivoLimpiarSuciedad t r = 
    crearTablero (largo t) (ancho t) 
    (suciedad t) (robots t) (cuna t) (ninhos t) 
    (obstaculos t) (cargados t) nuevosObjetivos (semilla t)
    where
        nObj = crearObjetivo objLimpiar r 1 (0,0) (x r) (y r)
        nuevosObjetivos = agregar nObj (objetivos t)

agregarObjetivoSuciedad :: Tablero -> Robot -> Int -> Int -> Int -> Tablero
agregarObjetivoSuciedad t r px py cost =
    _agregarObjetivo t nuevoObjetivo
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

_agregarObjetivo :: Tablero -> Objetivo -> Tablero
_agregarObjetivo t o
    | miCosto < rivalCosto =
        let nuevosObjetivos = (eliminar objetivoRival (objetivos t))

        in crearTablero (largo t) (ancho t) 
            (suciedad t) (robots t) (cuna t) 
            (ninhos t) (obstaculos t)
             (cargados t) (agregar o nuevosObjetivos) (semilla t)
    | otherwise = t
    where 
        px = objx o
        py = objy o
        objetivoRival = getObjetivo (objetivos t) px py
        miCosto = costo o
        rivalCosto = costo objetivoRival

getObjetivo :: [Objetivo] -> Int -> Int -> Objetivo
getObjetivo [] px py = crearObjetivo objCaminarHastaSuciedad (crearRobot 0 0) infinito (-1,-1) px py
getObjetivo (o:r) px py 
    | (objx o == px) && (objy o == py) = o
    | otherwise = getObjetivo r px py

crearObjetivoSuciedad :: Tablero -> Robot -> Int -> Int -> Int -> Objetivo
crearObjetivoSuciedad t r px py cost 
    | estaCargado t rx ry = error "notIMplementException"
    | otherwise = crearObjetivo objCaminarHastaSuciedad r (cost + 1) paso1 px py
    where 
        rx = x r
        ry = y r 
        paso1 = getPrimerPaso t px py r