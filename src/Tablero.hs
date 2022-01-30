module Tablero
    ( Tablero, iniciarTablero, crearTablero, get, estaVacio, hayObstaculo, noHayObstaculo,
    ninhoPuedeMoverse, hayNinho, estaCargado, robotPuedeMoverse,
    largo, ancho, suciedad, robots, cuna, ninhos, obstaculos, cargados
    ) where

import Elementos
import Listas

data Tablero = Tablero{largo::Int,
                        ancho::Int,
                         suciedad::[Suciedad],
                         robots::[Robot],
                         cuna::[Cuna],
                         ninhos::[Ninho],
                         obstaculos::[Obstaculo],
                         cargados :: [Cargado]
                         } deriving (Show, Eq)

crearTablero largo ancho suciedad robots cuna ninhos obstaculos cargados =
    Tablero largo ancho suciedad robots cuna ninhos obstaculos cargados


iniciarTablero :: Int->Int->Tablero
iniciarTablero largo ancho = Tablero largo ancho [] [] [] [] [] []

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

hayNinho :: Tablero -> Int -> Int -> Bool
hayNinho t x y = (get t x y) == tipoNinho

noEstaEnElCorral :: Tablero -> Ninho -> Bool
noEstaEnElCorral t n = not (pertenece (crearCuna (x n) (y n)) (cuna t))

ninhoPuedeMoverse :: Tablero -> Ninho -> Int -> Int -> Bool
ninhoPuedeMoverse t n xx yy = ((estaVacio t xx yy) || (hayCuna t xx yy)) && not(estaCargado t (x n) (y n)) && noEstaEnElCorral t n

robotPuedeMoverse :: Tablero -> Robot -> Int -> Int -> Bool
robotPuedeMoverse t r nx ny = (noHayObstaculo t nx ny) && not(pertenece (crearRobot nx ny) (robots t)) && (not (hayNinho t nx ny) || not (estaCargado t (x r) (y r)))

estaCargado t x y = pertenece (crearCargado x y) (cargados t)
