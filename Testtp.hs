module Testtp where

import Test.HUnit
import ArchivoDePruebas

-- Usuarios
usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")

-- Relaciones
relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) 
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)

-- Casos de Test Ejercicio 1
redTest2 = ([usuario1], [], [])
redTest3 = ([usuario1, usuario1, usuario2], [], [])
redTest4 = ([usuario1, usuario2, usuario3], [], [])

-- Casos de Test Ejercicio 2
redTest5 = ([usuario1, usuario2], [relacion1_2], [])
redTest6 = ([usuario1, usuario2], [relacion1_2, relacion1_2], [])

-- Los mas usados
redVacia = ([], [], [])


run = runTestTT testSuiteEjercicio4

testSuiteEjercicio1 = test [
    "Caso 1: Red vacía" ~: (nombresDeUsuarios redVacia) ~?= [],

    "Caso 2: Red con un usuario" ~: (nombresDeUsuarios redTest2) ~?= ["Juan"],

    "Caso 3: Red con nombres de usuarios repetidos" ~: (nombresDeUsuarios redTest3) ~?= ["Juan", "Maria"],

    "Caso 4: Red con nombres de usuarios que no se repiten" ~: (nombresDeUsuarios redTest4) ~?= ["Juan", "Julia", "Maria"]
        ]

testSuiteEjercicio2 = test [
    "Caso 1: Red vacía" ~: (amigosDe redVacia usuario1) ~?= [],

    "Caso 2: Red con un Usuario sin relaciones y ese usuario" ~: (amigosDe redTest2 usuario1) ~?= [],

    "Caso 3: Red con un Usuario sin relaciones y otro usuario" ~: (amigosDe redTest2 usuario2) ~?= [],

    "Caso 4: Red con Usuarios con una relacion u1 u2 y u1 " ~: (amigosDe redTest5 usuario1) ~?= [usuario2],

    "Caso 5: Red con Usuarios con una relacion u1 u2 y u2 " ~: (amigosDe redTest5 usuario2) ~?= [usuario1],

    "Caso 6: Red con Usuarios con una relacion repetida" ~: (amigosDe redTest6 usuario1) ~?= [usuario2]
        ] 

testSuiteEjercicio3 = test [
    "Caso 1: Red vacía" ~: (cantidadDeAmigos redVacia usuario1) ~?= 0
        ]

testSuiteEjercicio4 = test [
    "Caso 1: Usuario con 1 amigo" ~: (usuarioConMasAmigos redTest5) ~?= usuario1
        ]
{-
testSuiteEjercicio5 = test [
    "Caso 1: Red vacía" ~: (estaRobertoCarlos redVacia) ~?= []
        ]

testSuiteEjercicio6 = test [
    "Caso 1: Red vacía" ~: (publicacionesDe redVacia) ~?= []
        ]

testSuiteEjercicio7 = test [
    "Caso 1: Red vacía" ~: (publicacionesQueLeGustanA redVacia) ~?= []
        ]

testSuiteEjercicio8 = test [
    "Caso 1: Red vacía" ~: (lesGustanLasMismasPublicaciones redVacia) ~?= []
        ]

testSuiteEjercicio9 = test [
    "Caso 1: Red vacía" ~: (tieneUnSeguidorFiel redVacia) ~?= []
        ]

testSuiteEjercicio10 = test [
    "Caso 1: Red vacía" ~: (existeSecuenciaDeAmigos redVacia) ~?= []
        ]

-}