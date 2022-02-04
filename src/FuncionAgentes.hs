module FuncionAgentes
where

import Objetivos
import Tablero
import Elementos
import Escribir
import Movimiento

iteracion :: Tablero -> Tablero
iteracion t = let calcularObjetivos = bfs tSinObjetivos
              in trabajar calcularObjetivos (robots t)
              where
                  tSinObjetivos = limpiarObjetivos t (objetivos t)



trabajar :: Tablero -> [Robot] -> Tablero
trabajar t [] = t
trabajar t (r1:rr) = let nuevoT = muevete t r1
                     in trabajar nuevoT rr

muevete :: Tablero -> Robot -> Tablero
muevete t r = let mejorObjetivo = getMejorObjetivo objDeR r objHastaAhora
              in cumplirObjetivo t r mejorObjetivo
              where
                objDeR = filtrar (objetivos t) r []
                objHastaAhora = crearObjetivo objVagancia (crearRobot 0 0) infinito (-1,-1) 0 0

getMejorObjetivo :: [Objetivo] -> Robot -> Objetivo -> Objetivo
getMejorObjetivo [] r objGanador = objGanador 
getMejorObjetivo (o1:or) r objGanador 
        = getMejorObjetivo or r nuevoObjetivoGanador
            where 
                costoGanador = costo objGanador
                costoNuevo = costo o1
                nuevoObjetivoGanador = if costoNuevo < costoGanador
                                       then o1
                                       else objGanador

cumplirObjetivo :: Tablero -> Robot -> Objetivo -> Tablero
cumplirObjetivo t r o
    | tipo == objVagancia = t
    | tipo == objCaminarHastaSuciedad = cumplirObjetivoCaminarLimpiar t r o
    | tipo == objLimpiar = cumplirObjetivoLimpiar t (x r) (y r)
    | tipo == objCargarNinho = daElPaso t r p1X p1Y
    | tipo == objLlevarACuna = daElPaso t r p1X p1Y
    | tipo == objDejarEnLaCuna = dejarEnLaCuna t r
    | otherwise = t
    where 
        tipo = tipoObjetivo o
        paso1 = paso o
        p1X = fst paso1
        p1Y = snd paso1

cumplirObjetivoLimpiar :: Tablero -> Int -> Int -> Tablero
cumplirObjetivoLimpiar t sx sy = borrar t (crearSuciedad sx sy)

cumplirObjetivoCaminarLimpiar :: Tablero -> Robot -> Objetivo -> Tablero
cumplirObjetivoCaminarLimpiar t r o = daElPaso t r pasox pasoy
    where
        rx = x r
        ry = y r
        ox = objx o
        oy = objy o
        paso1 = paso o
        pasox = fst paso1
        pasoy = snd paso1

daElPaso :: Tablero -> Robot -> Int -> Int -> Tablero
daElPaso t r px py
    | robotPuedeMoverse t r px py = mover t r dx dy
    | otherwise = let nuevoT = bfsElemento tSinObjDeR r [(rx, ry)] [[rx, ry, 0]]
                   in muevete nuevoT r
     where
        rx = x r
        ry = y r
        dx = px - rx
        dy = py - ry
        objDeR = filtrar (objetivos t) r []
        tSinObjDeR = limpiarObjetivos t objDeR

dejarEnLaCuna :: Tablero -> Robot -> Tablero
dejarEnLaCuna t r = descargar t (x r) (y r)
