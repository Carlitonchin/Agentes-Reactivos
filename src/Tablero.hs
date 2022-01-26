module Tablero
    ( Tablero, iniciarTablero, crearTablero, get, estaVacio, hayObstaculo, noHayObstaculo,
    ninhoPuedeMoverse,
    largo, ancho, suciedad, robots, cuna, ninhos, obstaculos
    ) where

import Elementos
import Listas

data Tablero = Tablero{largo::Int,
                        ancho::Int,
                         suciedad::[Suciedad],
                         robots::[Robot],
                         cuna::[Cuna],
                         ninhos::[Ninho],
                         obstaculos::[Obstaculo]
                         } deriving (Show)

crearTablero largo ancho suciedad robots cuna ninhos obstaculos =
    Tablero largo ancho suciedad robots cuna ninhos obstaculos



iniciarTablero :: Int->Int->Tablero
iniciarTablero largo ancho = Tablero largo ancho [] [] [] [] []

get :: Tablero -> Int -> Int -> String
get tablero x y | pertenece (crearNinho x y) (ninhos tablero) = tipoNinho
                | pertenece (crearCuna x y) (cuna tablero) = tipoCuna
                | pertenece (crearObstaculo x y) (obstaculos tablero) = tipoObstaculo
                | pertenece (crearRobot x y) (robots tablero) = tipoRobot
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

ninhoPuedeMoverse t x y = (estaVacio t x y) || (hayCuna t x y)
