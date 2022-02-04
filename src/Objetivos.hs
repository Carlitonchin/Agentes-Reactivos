module Objetivos where

import Tablero
import Elementos
import Listas
import Movimiento
import Pintar
import GHC.Float
import Escribir

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
    | (estaCargado t xr yr) && arriba2X == xr && arriba2Y == yr && (robotPuedeMoverse t r px py) = True
    | (estaCargado t xr yr) && abajo2X == xr && abajo2Y == yr && (robotPuedeMoverse t r px py) = True
    | (estaCargado t xr yr) && izq2X == xr && izq2Y == yr && (robotPuedeMoverse t r px py) = True
    | (estaCargado t xr yr) && der2X == xr && der2Y == yr && (robotPuedeMoverse t r px py) = True
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
        arriba2X = px
        arriba2Y = nuevaY t robotImaginario (-2)
        abajo2X = px
        abajo2Y = nuevaY t robotImaginario (2)
        izq2X = nuevaY t robotImaginario (-2)
        izq2Y = py
        der2X = nuevaX t robotImaginario 2
        der2Y = py


iteracionBfs :: Tablero -> Int -> Int -> Robot -> [(Int, Int)] -> [[Int]] -> Int -> ([[Int]],[(Int,Int)])
iteracionBfs t px py r visitados cola cont
    | estaCargado t xr yr = anhadeALaCola t r [(arribaX, arribaY), (abajoX, abajoY), (izqX, izqY), (derX, derY), 
                                                 (arriba2X, arriba2Y), (abajo2X, abajo2Y), (izq2X, izq2Y), (der2X, der2Y)] cola visitados cont px py
    | otherwise = anhadeALaCola t r [(arribaX, arribaY), (abajoX, abajoY), (izqX, izqY), (derX, derY)] cola visitados cont px py
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
        arriba2X = px
        arriba2Y = nuevaY t robotImaginario (-2)
        abajo2X = px
        abajo2Y = nuevaY t robotImaginario (2)
        izq2X = nuevaX t robotImaginario (-2)
        izq2Y = py
        der2X = nuevaX t robotImaginario 2
        der2Y = py


anhadeALaCola :: Tablero -> Robot -> [(Int, Int)] -> [[Int]] -> [(Int, Int)] -> Int -> Int -> Int -> ([[Int]],[(Int,Int)])
anhadeALaCola t r [] cola visitados cont px py = (cola,visitados)
anhadeALaCola t r (f:rv) cola visitados cont px py = anhadeALaCola t r rv nuevaCola nuevoVisitados cont px py
    where 
        xf = fst f
        yf = snd f
        xr = x r
        yr = y r
        r2 = crearRobot px py
        tableroImaginario = escribir t r2
        t2 = if (estaCargado t xr yr) then cargar tableroImaginario px py else tableroImaginario
        nuevaColaVisitados = if (not (pertenece f visitados)) && (robotPuedeMoverse t2 r2 xf yf) && ((estaVacio t xf yf) || (haySuciedad t xf yf) || (hayCuna t xf yf) || ((hayNinho t xf yf) && (not (estaCargado t xr yr)) && (not (estaCargado t xf yf)) && (not (habemusCuna t xf yf))) )  
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
    | (habemusCuna t (x r1) (y r1)) && (estaCargado t (x r1) (y r1))= 
        let nTablero2 = agregarObjetivoDejarEnLaCuna t r1
        in bfsLista nTablero2 rr 

    | otherwise = bfsLista nuevoTablero rr
                    where
                        casillasMarcadas = [(x r1, y r1)]
                        cola = [[x r1, y r1, 0]]
                        nuevoTablero = bfsElemento t r1 casillasMarcadas cola


bfsElemento :: Tablero -> Robot -> [(Int,Int)] -> [[Int]]-> Tablero
bfsElemento t r marcadas [] = t
bfsElemento t r marcadas (pos:cola) = 
        bfsElemento nuevoTablero r nuevasMarcadas nuevaCola
        where
        xr = x r
        yr = y r
        px = indexar pos 0
        py = indexar pos 1
        cont = indexar pos 2
        contFloat = int2Float cont
        objetivost = objetivos t
        nuevoTablero = agregarObjetivo t r px py contFloat
        nuevosObjetivos = objetivos nuevoTablero
        nuevaColaVisitadas = iteracionBfs t px py r marcadas cola cont
        nuevaCola = fst nuevaColaVisitadas
        nuevasMarcadas = snd nuevaColaVisitadas

agregarObjetivo :: Tablero -> Robot -> Int -> Int -> Float-> Tablero
agregarObjetivo t r px py costo
    | tipoElemento == tipoSuciedad = agregarObjetivoSuciedad t r px py costo
    | tipoElemento == tipoNinho && not (estaCargado t xr yr) && not (habemusCuna t px py) = agregarObjetivoNinho t r px py costo
    | tipoElemento == tipoCuna && (estaCargado t xr yr) = agregarObjetivoCuna t r px py (costo/1.5)
    | otherwise = t
    where
        tipoElemento = get t px py
        xr = x r
        yr = y r

agregarObjetivoNinho :: Tablero -> Robot -> Int -> Int -> Float -> Tablero
agregarObjetivoNinho t r px py costo =
     _agregarObjetivo t nuevoObjetivo
    where
        nuevoObjetivo = crearObjetivoNinho t r px py costo

agregarObjetivoLimpiarSuciedad :: Tablero -> Robot -> Tablero
agregarObjetivoLimpiarSuciedad t r = 
    crearTablero (largo t) (ancho t) 
    (suciedad t) (robots t) (cuna t) (ninhos t) 
    (obstaculos t) (cargados t) nuevosObjetivos (semilla t)
    where
        nObj = crearObjetivo objLimpiar r 0 (0,0) (x r) (y r)
        nuevosObjetivos = agregar nObj (objetivos t)

agregarObjetivoDejarEnLaCuna :: Tablero -> Robot -> Tablero
agregarObjetivoDejarEnLaCuna t r =
    crearTablero (largo t) (ancho t) 
    (suciedad t) (robots t) (cuna t) (ninhos t) 
    (obstaculos t) (cargados t) nuevosObjetivos (semilla t)
    where
        nObj = crearObjetivo objDejarEnLaCuna r 0 (0,0) (x r) (y r)
        nuevosObjetivos = agregar nObj (objetivos t)

agregarObjetivoSuciedad :: Tablero -> Robot -> Int -> Int -> Float -> Tablero
agregarObjetivoSuciedad t r px py cost =
    _agregarObjetivo t nuevoObjetivo
    where
        nuevoObjetivo = crearObjetivoSuciedad t r px py cost

agregarObjetivoCuna :: Tablero -> Robot -> Int -> Int -> Float -> Tablero
agregarObjetivoCuna t r px py cost =
    _agregarObjetivo t nuevoObjetivo
    where
        nuevoObjetivo = crearObjetivoCuna t r px py cost

_agregarObjetivo :: Tablero -> Objetivo -> Tablero
_agregarObjetivo t o
    | miCosto < rivalCosto =
        
        let nuevosObjetivos = (eliminar objetivoRival (objetivos t))
            nuevoTablero = crearTablero (largo t) (ancho t) (suciedad t) (robots t) (cuna t) (ninhos t) (obstaculos t) (cargados t) (agregar o nuevosObjetivos) (semilla t)
        
        in  if (tipoObjetivo objetivoRival) /= objFantasma -- && robotRival /= miRobot
            then bfsElemento nuevoTablero robotRival [(xrr, yrr)] [[xrr, yrr, 0]]
            else nuevoTablero            
    
    | otherwise = t
    where 
        px = objx o
        py = objy o
        miCosto = costo o
        miRobot = robot o
        objetivoRival = getObjetivo (objetivos t) miRobot px py
        
        rivalCosto = costo objetivoRival
        robotRival = robot objetivoRival
        xrr = x robotRival
        yrr = y robotRival

getObjetivo :: [Objetivo] -> Robot -> Int -> Int -> Objetivo
getObjetivo [] r px py = crearObjetivo objFantasma (crearRobot 0 0) infinito (-1,-1) px py
getObjetivo (o:ro) r px py 
    | ((objx o == px) && (objy o == py)) || ((robot o) == r) = o
    | otherwise = getObjetivo ro r px py

crearObjetivoSuciedad :: Tablero -> Robot -> Int -> Int -> Float -> Objetivo
crearObjetivoSuciedad t r px py cost 
    | estaCargado t rx ry = crearObjetivo objCaminarHastaSuciedad r (cost + 1) paso1 px py
    | otherwise = crearObjetivo objCaminarHastaSuciedad r (cost + 1) paso1 px py
    where 
        rx = x r
        ry = y r 
        paso1 = getPrimerPaso t px py r

crearObjetivoCuna :: Tablero -> Robot -> Int -> Int -> Float -> Objetivo
crearObjetivoCuna t r px py cost
    = crearObjetivo objLlevarACuna r (cost + 1) paso1 px py
    where
        rx = x r
        ry = y r
        paso1 = getPrimerPaso t px py r

crearObjetivoNinho :: Tablero -> Robot -> Int -> Int -> Float -> Objetivo
crearObjetivoNinho t r px py cost =
    crearObjetivo objCargarNinho r cost paso1 px py
    where 
        rx = x r
        ry = y r 
        paso1 = getPrimerPaso t px py r