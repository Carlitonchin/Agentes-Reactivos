module Tablero
    ( Tablero,
    Cuna
    ) where

import Elementos

data Tablero = Tablero{largo::Int,
                        ancho::Int,
                         suciedad::[Suciedad],
                         robots::[Robot],
                         cuna::[Cuna],
                         nihos::[Ninho],
                         obstaculos::[Obstaculo]
                         }