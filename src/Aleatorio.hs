module Aleatorio where

import Tablero
import Escribir
import Elementos
import Listas

random :: Tablero -> Tablero
random t = crearTablero (largo t) (ancho t) (suciedad t) (robots t) (cuna t) (ninhos t) (obstaculos t) (cargados t) (generar (semilla t))

generar :: Int -> Int
generar s = div (div (div ((s*25+44) * 5) 2) 3)  2

randomNum :: Int -> Int -> Int
randomNum max sem = abs (mod sem max)

getPosicion :: Tablero -> [[Int]] -> [Int]
getPosicion t espaciosVacios = let rNum = randomNum (length espaciosVacios) (semilla t)
                               in indexarLista espaciosVacios rNum

esAdyacenteAEsta :: Int -> Int -> Int -> Int -> Bool
esAdyacenteAEsta x y xf yf | [x+1, y] == [xf, yf] = True
                           | [x-1, y] == [xf, yf] = True 
                           | [x, y+1] == [xf, yf] = True
                           | [x, y-1] == [xf, yf] = True
                           | otherwise = False 

esAdyacente :: Int -> Int -> [[Int]] -> Bool
esAdyacente x y [] = False
esAdyacente x y (f:r) = let xf = indexar f 0
                            yf = indexar f 1
                        in (esAdyacenteAEsta x y xf yf) || (esAdyacente x y r)

casillasAdyacentesParaCuna :: Tablero -> Int -> [[Int]] -> [[Int]] -> (Tablero, [[Int]])
casillasAdyacentesParaCuna t cant seleccionadas disponibles | cant == 0 = (t,seleccionadas)
                                                            | length seleccionadas == 0 = casillasAdyacentesParaCuna tRandom (cant - 1) (agregar nueva seleccionadas) (eliminar nueva espaciosVacios)
                                                            | esAdyacente nx ny seleccionadas = casillasAdyacentesParaCuna tRandom (cant - 1) (agregar nueva seleccionadas) (eliminar nueva espaciosVacios)
                                                            | otherwise = casillasAdyacentesParaCuna tRandom cant seleccionadas (eliminar nueva disponibles)
                                                            where tRandom = random t
                                                                  nuevaPosicion = getPosicion tRandom disponibles
                                                                  nx = indexar nuevaPosicion 0
                                                                  ny = indexar nuevaPosicion 1
                                                                  nueva = [nx, ny] 
                                                                  espaciosVacios = quitarTodos seleccionadas (getEspaciosVacios tRandom)

pintarCunas :: Tablero -> [[Int]] -> Tablero
pintarCunas t [] = t 
pintarCunas t (f:r) = let nuevoTablero = escribir t (crearCuna (indexar f 0) (indexar f 1))
                      in pintarCunas nuevoTablero r  

generarCuna :: Tablero -> Int -> Tablero
generarCuna t cant = let disponibles = getEspaciosVacios t 
                         seleccionadas = []
                         listas = casillasAdyacentesParaCuna t cant seleccionadas disponibles
                    in pintarCunas (fst listas) (snd listas)