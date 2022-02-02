module Tablero
where

import Elementos
import Listas

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
                         } deriving (Show)

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
robotPuedeMoverse t r nx ny = (noHayObstaculo t nx ny) && not(pertenece (crearRobot nx ny) (robots t)) && (not (hayNinho t nx ny) || not (estaCargado t (x r) (y r)))

estaCargado t x y = pertenece (crearCargado x y) (cargados t)

getEspaciosVacios :: Tablero -> [[Int]]
getEspaciosVacios t = getEspaciosVaciosRecursivo t 0 0

getEspaciosVaciosRecursivo :: Tablero -> Int -> Int -> [[Int]]
getEspaciosVaciosRecursivo t px py | py == largo t = []
                                   | px == ancho t = getEspaciosVaciosRecursivo t 0 (py+1)
                                   | estaVacio t px py = [[px, py]] ++ (getEspaciosVaciosRecursivo t (px + 1) py) 
                                   | otherwise = getEspaciosVaciosRecursivo t (px + 1) py
