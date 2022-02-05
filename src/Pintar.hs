module Pintar 
 where

import Elementos
import Listas
import Tablero

soloNinhoText emoji = if emoji then "⬜👦⬜" else "| N |"
soloRobotText emoji = if emoji then "⬜🤖⬜" else "| R |"
soloCunaText emoji = if emoji then "⬜🏠⬜" else "| C |"
obstaculoText emoji = if emoji then "⬜⬛⬜" else "| O |"
soloSuciedadText emoji = if emoji then "⬜💩⬜" else "| S |"
robotCargandoNinhoText emoji = if emoji then "🤖👦⬜" else "|R N|"
robotNinhoText emoji = if emoji then "👦🤖⬜" else "|N R|"
ninhoCunaText emoji = if emoji then "👦🏠⬜" else "|N C|"
robotCunaText emoji = if emoji then "🤖🏠⬜" else "|R C|"
robotCargandoNinhoEnCunaText emoji = if emoji then "🤖👦🏠" else "|RNC|"
robotNinhoCunaText emoji = if emoji then "👦🤖🏠" else "|NRC|"
robotSuciedadText emoji = if emoji then "🤖💩⬜" else "|R S|"
robotCargandoNinhoEnLaSuciedadText emoji = if emoji then "🤖👦💩" else "|RNS|"
robotConNinhoEnLaSuciedadText emoji = if emoji then "👦🤖💩" else "|NRS|"
ninhoEnLaSuciedadText emoji = if emoji then "👦💩⬜" else "|N S|"
vacioText emoji = if emoji then "⬜⬜⬜" else "|   |"

habemusCuna :: Tablero -> Int -> Int -> Bool
habemusCuna t x y = pertenece (crearCuna x y) (cuna t)

habemusRobot :: Tablero -> Int -> Int -> Bool
habemusRobot t x y = pertenece (crearRobot x y) (robots t)

habemusSuciedad :: Tablero -> Int -> Int -> Bool
habemusSuciedad t x y = pertenece (crearSuciedad x y) (suciedad t)

pintarElemento :: Posicion p => Tablero -> p -> Bool -> String
pintarElemento t p emoji | tipo p == tipoNinho = pintarNinho t (toNinho p) emoji
                   | tipo p == tipoRobot = pintarRobot t (toRobot p)  emoji
                   | tipo p == tipoCuna = soloCunaText emoji
                   | tipo p == tipoSuciedad = soloSuciedadText emoji
                   | tipo p == tipoObstaculo = obstaculoText emoji
                   | otherwise = vacioText emoji

pintarNinho :: Tablero -> Ninho -> Bool -> String
pintarNinho t n emoji | habemusCuna t xn yn && habemusRobot t xn yn && estaCargado t xn yn = robotCargandoNinhoEnCunaText emoji
                | habemusCuna t xn yn && habemusRobot t xn yn = robotNinhoCunaText emoji
                | habemusRobot t xn yn && habemusSuciedad t xn yn && estaCargado t xn yn = robotCargandoNinhoEnLaSuciedadText emoji
                | habemusRobot t xn yn && habemusSuciedad t xn yn = robotConNinhoEnLaSuciedadText emoji
                | habemusRobot t xn yn && estaCargado t xn yn = robotCargandoNinhoText emoji
                | habemusRobot t xn yn = robotNinhoText emoji
                | habemusSuciedad t xn yn = ninhoEnLaSuciedadText emoji
                | habemusCuna t xn yn = ninhoCunaText emoji
                | otherwise = soloNinhoText emoji
                where xn = x n 
                      yn = y n

pintarRobot :: Tablero -> Robot -> Bool -> String
pintarRobot t r emoji | habemusCuna t xr yr = robotCunaText emoji
                | habemusSuciedad t xr yr = robotSuciedadText emoji
                | otherwise = soloRobotText emoji
                where xr = x r 
                      yr = y r

encontrarPintarElemento :: Tablero -> Int -> Int -> Bool -> String
encontrarPintarElemento t x y emoji | get t x y == tipoNinho = pintarNinho t (crearNinho x y) emoji
                              | get t x y == tipoRobot = pintarRobot t (crearRobot x y) emoji
                              | get t x y == tipoCuna = pintarElemento t (crearCuna x y) emoji
                              | get t x y == tipoObstaculo = pintarElemento t (crearObstaculo x y) emoji
                              | get t x y == tipoSuciedad = pintarElemento t (crearSuciedad x y) emoji
                              | otherwise = pintarElemento t crearVacio emoji

pintarFilaTablero :: Tablero -> Int -> Int -> Bool -> String
pintarFilaTablero t x y emoji | y == largo t = ""
                        | x == ancho t = "\n\n" ++ (pintarFilaTablero t 0 (y+1) emoji)
                        | otherwise = (encontrarPintarElemento t x y emoji) ++ " " ++ (pintarFilaTablero t (x+1) y emoji)

pintarTablero :: Tablero -> Bool -> String
pintarTablero t emoji = pintarFilaTablero t 0 0 emoji