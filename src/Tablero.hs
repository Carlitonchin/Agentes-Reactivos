module Tablero
where

import Elementos
import Listas
import GHC.Int

data Tablero = Tablero{largo::Int,
                        ancho::Int,
                         suciedad::[Suciedad],
                         robots::[Robot],
                         cuna::[Cuna],
                         ninhos::[Ninho],
                         obstaculos::[Obstaculo],
                         cargados :: [Cargado],
                         objetivos :: [Objetivo],
                         semilla :: Int
                         } deriving (Show, Eq)

crearTablero largo ancho suciedad robots cuna ninhos obstaculos cargados objetivos semilla =
    Tablero largo ancho suciedad robots cuna ninhos obstaculos cargados objetivos semilla


iniciarTablero :: Int -> Int -> Int ->Tablero
iniciarTablero largo ancho semilla = Tablero largo ancho [] [] [] [] [] [] [] semilla

get :: Tablero -> Int -> Int -> String
get tablero x y | pertenece (crearNinho x y) (ninhos tablero) = tipoNinho
                | pertenece (crearRobot x y) (robots tablero) = tipoRobot
                | pertenece (crearCuna x y) (cuna tablero) = tipoCuna
                | pertenece (crearObstaculo x y) (obstaculos tablero) = tipoObstaculo
                | pertenece (crearSuciedad x y) (suciedad tablero) = tipoSuciedad
                | otherwise = tipoVacio


estaVacio :: Tablero -> Int -> Int -> Bool
estaVacio tablero x y = (get tablero x y) == tipoVacio

hayObstaculo :: Tablero -> Int -> Int -> Bool
hayObstaculo tablero x y = (get tablero x y) == tipoObstaculo

noHayObstaculo :: Tablero -> Int -> Int -> Bool
noHayObstaculo tablero x y = not (hayObstaculo tablero x y)

hayCuna :: Tablero -> Int -> Int -> Bool
hayCuna t x y = (get t x y) == tipoCuna

haySuciedad :: Tablero -> Int -> Int -> Bool
haySuciedad t x y = (get t x y) == tipoSuciedad

hayNinho :: Tablero -> Int -> Int -> Bool
hayNinho t x y = (get t x y) == tipoNinho

noEstaEnElCorral :: Tablero -> Ninho -> Bool
noEstaEnElCorral t n = not (pertenece (crearCuna (x n) (y n)) (cuna t))

ninhoPuedeMoverse :: Tablero -> Ninho -> Int -> Int -> Bool
ninhoPuedeMoverse t n xx yy = (estaVacio t xx yy) && (not(estaCargado t (x n) (y n))) && (noEstaEnElCorral t n)

robotPuedeMoverse :: Tablero -> Robot -> Int -> Int -> Bool
robotPuedeMoverse t r nx ny
    | modx <= 1 && mody <= 1 = (noHayObstaculo t nx ny) && not(pertenece (crearRobot nx ny) (robots t)) && (not (hayNinho t nx ny) || not (estaCargado t (x r) (y r)))
    | otherwise = (estaCargado t xr yr) && (robotPuedeMoverse t r n1x n1y) && (robotPuedeMoverse t2 r2 nx ny)
    where
        xr = x r
        yr = y r
        dx = nx - xr
        dy = ny - yr
        modx = abs (nx - xr)
        mody = abs (ny - yr)
        ndx = if dx == 0 then 0 else (div dx (abs (dx)))
        ndy = if dy == 0 then 0 else (div dy (abs (dy)))
        n1x = xr + ndx
        n1y = yr + ndy
        r2 = crearRobot n1x n1y
        nuevoCargados = if estaCargado t xr yr
                        then agregar (crearCargado n1x n1y) (cargados t)
                        else cargados t
        nuevoRobots = agregar r2 (robots t)
        t2 = Tablero (largo t) (ancho t) (suciedad t) nuevoRobots (cuna t) (ninhos t) (obstaculos t) nuevoCargados (objetivos t) (semilla t)

estaCargado t x y = pertenece (crearCargado x y) (cargados t)

getEspaciosVacios :: Tablero -> [[Int]]
getEspaciosVacios t = getEspaciosVaciosRecursivo t 0 0

getEspaciosLlenosTupla :: Tablero -> [(Int, Int)]
getEspaciosLlenosTupla t = getEspaciosLlenosTuplaRecursivo t 0 0

getEspaciosLlenosTuplaRecursivo :: Tablero -> Int -> Int -> [(Int, Int)]
getEspaciosLlenosTuplaRecursivo t px py 
    | py == largo t = []
    | px == ancho t = getEspaciosLlenosTuplaRecursivo t 0 (py+1)
    | not (estaVacio t px py) = [(px, py)] ++ (getEspaciosLlenosTuplaRecursivo t (px + 1) py) 
    | otherwise = getEspaciosLlenosTuplaRecursivo t (px + 1) py

getEspaciosVaciosRecursivo :: Tablero -> Int -> Int -> [[Int]]
getEspaciosVaciosRecursivo t px py | py == largo t = []
                                   | px == ancho t = getEspaciosVaciosRecursivo t 0 (py+1)
                                   | estaVacio t px py = [[px, py]] ++ (getEspaciosVaciosRecursivo t (px + 1) py) 
                                   | otherwise = getEspaciosVaciosRecursivo t (px + 1) py

ninhosNoCargadosNiEnCuna :: Tablero -> [Ninho] -> [Ninho] -> [Ninho]
ninhosNoCargadosNiEnCuna t [] acumulado = acumulado
ninhosNoCargadosNiEnCuna t (n1:rn) acumulado =
    let nuevoAcumulado = if (not (estaCargado t (x n1) (y n1))) && (noEstaEnElCorral t n1)
                         then agregar n1 acumulado
                         else acumulado
    in ninhosNoCargadosNiEnCuna t rn nuevoAcumulado 