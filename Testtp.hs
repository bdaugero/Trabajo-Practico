module Testtp where

import Test.HUnit
import Tp1

-- Casos de Test

run = runTestTT testSuiteEj1

testSuiteEj1 = test [
    "Caso 1: Red vacía" ~: (nombresDeUsuarios redTest1) ~?= [],
    "Caso 2: Red con un usuario" ~: (nombresDeUsuarios redTest2) ~?= ["Juan"],
    "Caso 3: Red con nombres de usuarios repetidos" ~: (nombresDeUsuarios redTest3) ~?= ["Juan", "Maria"],
    "Caso 4: Red con nombres de usuarios que no se repiten" ~: (nombresDeUsuarios redTest4) ~?= ["Juan", "Julia", "Maria"]
        ]

redTest1 = ([], [], [])
redTest2 = ([(1,"Juan")], [], [])
redTest3 = ([(1,"Juan"), (2, "Juan"), (3, "Maria")], [], [])
redTest4 = ([(1,"Juan"), (2, "Julia"), (3, "Maria")], [], [])
