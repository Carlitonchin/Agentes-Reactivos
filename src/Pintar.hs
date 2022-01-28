module Pintar (soloNinhoText, soloRobotText, soloCunaText, obstaculoText, soloSuciedadText,
                pintarTablero                                                    )
 where

import Elementos
import Listas
import Tablero

soloNinhoText = "⬜👦⬜"
soloRobotText = "⬜🤖⬜"
soloCunaText = "⬜🏠⬜"
obstaculoText = "⬜⬛⬜"
soloSuciedadText = "⬜💩⬜"
robotCargandoNinhoText = "🤖👦⬜"
robotNinhoText = "👦🤖⬜"
ninhoCunaText = "👦🏠⬜"
robotCunaText = "🤖🏠⬜"
robotCargandoNinhoEnCunaText = "🤖👦🏠"
robotNinhoCunaText = "👦🤖🏠"
robotSuciedadText = "🤖💩⬜"
robotCargandoNinhoEnLaSuciedadText = "🤖👦💩"
robotConNinhoEnLaSuciedadText = "👦🤖💩"
ninhoEnLaSuciedadText = "👦💩⬜"
vacioText = "⬜⬜⬜"

habemusCuna :: Tablero -> Int -> Int -> Bool
habemusCuna t x y = pertenece (crearCuna x y) (cuna t)

habemusRobot :: Tablero -> Int -> Int -> Bool
habemusRobot t x y = pertenece (crearRobot x y) (robots t)

habemusSuciedad :: Tablero -> Int -> Int -> Bool
habemusSuciedad t x y = pertenece (crearSuciedad x y) (suciedad t)

pintarElemento :: Posicion p => Tablero -> p -> String
pintarElemento t p | tipo p == tipoNinho = pintarNinho t (toNinho p)
                   | tipo p == tipoRobot = pintarRobot t (toRobot p) 
                   | tipo p == tipoCuna = soloCunaText
                   | tipo p == tipoSuciedad = soloSuciedadText
                   | tipo p == tipoObstaculo = obstaculoText
                   | otherwise = vacioText

pintarNinho :: Tablero -> Ninho -> String
pintarNinho t n | habemusCuna t xn yn && habemusRobot t xn yn && estaCargado t xn yn = robotCargandoNinhoEnCunaText
                | habemusCuna t xn yn && habemusRobot t xn yn = robotNinhoCunaText
                | habemusRobot t xn yn && habemusSuciedad t xn yn && estaCargado t xn yn = robotCargandoNinhoEnLaSuciedadText
                | habemusRobot t xn yn && habemusSuciedad t xn yn = robotConNinhoEnLaSuciedadText
                | habemusRobot t xn yn && estaCargado t xn yn = robotCargandoNinhoText
                | habemusRobot t xn yn = robotNinhoText
                | habemusSuciedad t xn yn = ninhoEnLaSuciedadText
                | habemusCuna t xn yn = ninhoCunaText
                | otherwise = soloNinhoText
                where xn = x n 
                      yn = y n

pintarRobot :: Tablero -> Robot -> String
pintarRobot t r | habemusCuna t xr yr = robotCunaText
                | habemusSuciedad t xr yr = robotSuciedadText
                | otherwise = soloRobotText
                where xr = x r 
                      yr = y r

encontrarPintarElemento :: Tablero -> Int -> Int -> String
encontrarPintarElemento t x y | get t x y == tipoNinho = pintarNinho t (crearNinho x y)
                              | get t x y == tipoRobot = pintarRobot t (crearRobot x y)
                              | get t x y == tipoCuna = pintarElemento t (crearCuna x y)
                              | get t x y == tipoObstaculo = pintarElemento t (crearObstaculo x y)
                              | get t x y == tipoSuciedad = pintarElemento t (crearSuciedad x y)
                              | otherwise = pintarElemento t crearVacio

pintarFilaTablero :: Tablero -> Int -> Int -> String
pintarFilaTablero t x y | y == largo t = ""
                        | x == ancho t = "\n\n" ++ (pintarFilaTablero t 0 (y+1))
                        | otherwise = (encontrarPintarElemento t x y) ++ " " ++ (pintarFilaTablero t (x+1) y)

pintarTablero :: Tablero -> String
pintarTablero t = pintarFilaTablero t 0 0