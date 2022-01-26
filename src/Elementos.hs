module Elementos
    ( 
    crearNinho, crearRobot, crearSuciedad, crearCuna, crearObstaculo, tipo,
    tipoNinho, tipoCuna, tipoObstaculo, tipoRobot, tipoSuciedad,tipoVacio,
    toNinho, toRobot, toObstaculo, toSuciedad, toCuna,
    Ninho, Robot, Suciedad, Cuna, Obstaculo, Posicion,
    x, y
    ) where

class Posicion p where
    x :: p -> Int
    y :: p -> Int
    tipo :: p -> String
    toNinho :: p -> Ninho
    toRobot :: p -> Robot
    toObstaculo :: p -> Obstaculo
    toSuciedad :: p -> Suciedad
    toCuna :: p -> Cuna

data Ninho = Ninho {xNinho::Int, yNinho::Int} deriving(Show, Eq)
data Robot = Robot {xRobot::Int, yRobot::Int} deriving(Show, Eq)
data Suciedad = Suciedad {xSuciedad::Int, ySuciedad::Int} deriving(Show, Eq)
data Cuna = Cuna {xCuna::Int, yCuna::Int} deriving(Show, Eq)
data Obstaculo = Obstaculo {xObstaculo::Int, yObstaculo::Int} deriving(Show, Eq)

tipoNinho = "Ninho"
tipoRobot = "Robot"
tipoSuciedad = "Suciedad"  
tipoCuna = "Cuna"
tipoObstaculo = "Obstaculo"
tipoVacio = "Vacio"

crearNinho :: Int->Int -> Ninho
crearNinho x y = Ninho x y
crearRobot x y = Robot x y
crearSuciedad x y = Suciedad x y
crearCuna x y = Cuna x y
crearObstaculo x y = Obstaculo x y

instance Posicion Ninho where
    x a = xNinho a
    y a = yNinho a
    tipo a = tipoNinho
    toNinho a = crearNinho (x a) (y a)


instance Posicion Robot where
    x a = xRobot a
    y a = yRobot a
    tipo a = tipoRobot
    toRobot a = crearRobot (x a) (y a)

instance Posicion Suciedad where
    x a = xSuciedad a
    y a = ySuciedad a
    tipo a = tipoSuciedad
    toSuciedad a = crearSuciedad (x a) (y a)

instance Posicion Cuna where
    x a = xCuna a
    y a = yCuna a
    tipo a = tipoCuna
    toCuna a = crearCuna (x a) (y a)

instance Posicion Obstaculo where
    x a = xObstaculo a
    y a = yObstaculo a
    tipo a = tipoObstaculo
    toObstaculo a = crearObstaculo (x a) (y a)