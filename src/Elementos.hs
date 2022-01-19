module Elementos
    ( 
    crearNinho, crearRobot, crearSuciedad, crearCuna, crearObstaculo,
    Ninho, Robot, Suciedad, Cuna, Obstaculo,
    x, y
    ) where

class Posicion p where
    x :: p -> Int
    y :: p -> Int

data Ninho = Ninho {xNinho::Int, yNinho::Int}
data Robot = Robot {xRobot::Int, yRobot::Int}
data Suciedad = Suciedad {xSuciedad::Int, ySuciedad::Int}
data Cuna = Cuna {xCuna::Int, yCuna::Int}
data Obstaculo = Obstaculo {xObstaculo::Int, yObstaculo::Int}

crearNinho x y = Ninho x y
crearRobot x y = Robot x y
crearSuciedad x y = Suciedad x y
crearCuna x y = Cuna x y
crearObstaculo x y = crearObstaculo x y

instance Posicion Ninho where
    x a = xNinho a
    y a = yNinho a

instance Posicion Robot where
    x a = xRobot a
    y a = yRobot a

instance Posicion Suciedad where
    x a = xSuciedad a
    y a = ySuciedad a

instance Posicion Cuna where
    x a = xCuna a
    y a = yCuna a

instance Posicion Obstaculo where
    x a = xObstaculo a
    y a = yObstaculo a