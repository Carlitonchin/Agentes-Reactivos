module Tablero
    ( Tablero, iniciarTablero, crearTablero,
    largo, ancho, suciedad, robots, cuna, ninhos, obstaculos
    ) where

import Elementos

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

rellenarTablero :: Tablero->Tablero
rellenarTablero t = t