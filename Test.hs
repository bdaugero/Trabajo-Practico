module Test where

import Test.HUnit
import Solucion

main = runTestTT todosLosTests

todosLosTests = test [testSuiteEjercicio1, testSuiteEjercicio2, testSuiteEjercicio3, testSuiteEjercicio4, testSuiteEjercicio5, testSuiteEjercicio6, testSuiteEjercicio7, testSuiteEjercicio8, testSuiteEjercicio9, testSuiteEjercicio10]

run1 = runTestTT testSuiteEjercicio1
run2 = runTestTT testSuiteEjercicio2
run3 = runTestTT testSuiteEjercicio3
run4 = runTestTT testSuiteEjercicio4
run5 = runTestTT testSuiteEjercicio5
run6 = runTestTT testSuiteEjercicio6
run7 = runTestTT testSuiteEjercicio7
run8 = runTestTT testSuiteEjercicio8
run9 = runTestTT testSuiteEjercicio9
run10 = runTestTT testSuiteEjercicio10


-- Usuarios
usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")
usuario6 = (6, "Bautista")
usuario7 = (7, "Tomas")
usuario8 = (8, "Jorge")
usuario9 = (9, "Antonella")
usuario10 = (10, "Julieta")
usuario11 = (11, "Carla")
usuario12 = (12, "Ramiro")

-- Relaciones
relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario1, usuario4)
relacion1_5 = (usuario1, usuario5)
relacion1_6 = (usuario1, usuario6)
relacion1_7 = (usuario1, usuario7)
relacion1_8 = (usuario1, usuario8)
relacion1_9 = (usuario1, usuario9)
relacion1_10 = (usuario1, usuario10)
relacion1_11 = (usuario1, usuario11)
relacion1_12 = (usuario1, usuario12)
relacion2_3 = (usuario2, usuario3)
relacion2_4 = (usuario2, usuario4)
relacion2_5 = (usuario2, usuario5)
relacion2_6 = (usuario2, usuario6)
relacion2_7 = (usuario2, usuario7)
relacion2_8 = (usuario2, usuario8)
relacion2_9 = (usuario2, usuario9)
relacion2_10 = (usuario2, usuario10)
relacion2_11 = (usuario2, usuario11)
relacion2_12= (usuario2, usuario12)
relacion3_4 = (usuario3, usuario4)


-- Publicaciones
publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario5])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])
publicacion1_5 = (usuario1, "Este es como mi quinto post", [usuario5])
publicacion1_6 = (usuario1, "Este es como mi sexto post", [])

publicacion2_1 = (usuario2, "Hello World", [usuario4])
publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario4])

publicacion3_1 = (usuario3, "Lorem Ipsum", [])
publicacion3_2 = (usuario3, "dolor sit amet", [usuario2])
publicacion3_3 = (usuario3, "consectetur adipiscing elit", [usuario2, usuario5])

publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
publicacion4_2 = (usuario4, "I am Bob", [])
publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario8])

publicacion5_1 = (usuario5, "Acabo de ver una película increíble", [usuario2])
publicacion5_2 = (usuario5, "¡Feliz cumpleaños, amigo!", [usuario6, usuario7])
publicacion5_3 = (usuario5, "No puedo creer que haya pasado un año desde que nos graduamos", [usuario2, usuario10])

publicacion6_1 = (usuario6, "Estoy emocionado por el concierto de esta noche", [usuario1, usuario3])
publicacion6_2 = (usuario6, "¡Este es el mejor día de mi vida!", [])
publicacion6_3 = (usuario6, "Tengo que estudiar mucho para el examen de mañana", [usuario1, usuario2, usuario3])

publicacion7_1 = (usuario7, "Hoy fue un día difícil, pero estoy agradecido por mi familia y amigos", [usuario1, usuario3])
publicacion7_2 = (usuario7, "Estoy muy feliz de haber encontrado un nuevo trabajo", [usuario10])
publicacion7_3 = (usuario7, "Voy a extrañar mucho a mis compañeros de trabajo", [usuario8, usuario10, usuario9])
publicacion7_4 = (usuario7, "Te amo mama", [])

publicacion8_1 = (usuario8, "Me encanta cocinar nuevos platos", [usuario7])
publicacion8_2 = (usuario8, "¡Qué hermoso día hace hoy!", [usuario9])
publicacion8_3 = (usuario8, "Estoy emocionado por el viaje que tengo planeado para el próximo mes", [usuario7])

publicacion9_1 = (usuario9, "No puedo esperar a que llegue el fin de semana", [usuario1, usuario2])
publicacion9_2 = (usuario9, "Acabo de terminar de leer un gran libro", [])
publicacion9_3 = (usuario9, "¡Felicitaciones a mi hermana por su nuevo trabajo!", [usuario9])

publicacion10_1 = (usuario10, "¿Alguien quiere ir al parque conmigo?", [usuario9, usuario10])
publicacion10_2 = (usuario10, "Estoy aburrido en casa, ¿alguien quiere jugar videojuegos?", [usuario7])
publicacion10_3 = (usuario10, "Estoy tan emocionado por la boda de mi mejor amigo este fin de semana", [usuario6, usuario3])

-- Redes
redVacia = ([], [], [])

-- Casos de Test Ejercicio 1
redTest1_1 = redVacia
redTest1_2 = ([usuario1], [], [])
redTest1_3 = ([usuario1, usuario2, usuario5], [], [])
redTest1_4 = ([usuario1, usuario2, usuario3, usuario4], [], [])

-- Ejercicio 1
testSuiteEjercicio1 = test [
    "Caso 1: Red vacía" ~: (nombresDeUsuarios redTest1_1) ~?= [],

    "Caso 2: Red con un usuario" ~: (nombresDeUsuarios redTest1_2) ~?= ["Juan"],

    "Caso 3: Red con varios usuarios y nombres repetidos" ~: (nombresDeUsuarios redTest1_3) ~?= ["Juan", "Natalia"],

    "Caso 4: Red con varios usuarios, sin nombres repetidos" ~: (nombresDeUsuarios redTest1_4) ~?= ["Juan", "Natalia", "Pedro", "Mariela"]
        ]


-- Casos de Test Ejercicio 2
redTest2_1 = ([usuario1],[],[])
redTest2_2 = ([usuario1, usuario2, usuario3], [relacion2_3], [])
redTest2_3 = ([usuario1, usuario2, usuario3],[],[])
redTest2_4 = ([usuario1, usuario2, usuario3],[relacion1_2], [])
redTest2_5 = ([usuario1, usuario2, usuario3],[relacion1_2, relacion1_3], [])
redTest2_6 = ([usuario1, usuario2],[relacion1_2],[])

-- Ejercicio 2
testSuiteEjercicio2 = test [
    "Caso 1: Red con un usuario" ~: (amigosDe redTest2_1 usuario1) ~?= [],

    "Caso 2: Red con varios usuarios y usuario sin relaciones" ~: (amigosDe redTest2_2 usuario1) ~?= [],

    "Caso 3: Red con varios usuarios y no hay relaciones" ~: (amigosDe redTest2_3 usuario1) ~?= [],

    "Caso 4: Red con varios usuarios y usuario con una relacion" ~: (amigosDe redTest2_4 usuario1) ~?= [usuario2],

    "Caso 5: Red con varios usuarios y usuario con varias relaciones" ~: (amigosDe redTest2_5 usuario1) ~?= [usuario2, usuario3],

    "Caso 6: Red con dos usuarios y una relacion entre ambos" ~: (amigosDe redTest2_6 usuario1) ~?= [usuario2]
        ] 


-- Casos de Test Ejercicio 3
redTest3_1 = ([usuario1],[],[])
redTest3_2 = ([usuario1, usuario2, usuario3], [relacion2_3], [])
redTest3_3 = ([usuario1, usuario2, usuario3], [relacion1_2, relacion2_3], [])
redTest3_4 = ([usuario1, usuario2, usuario3], [relacion1_2, relacion1_3, relacion2_3], [])


-- Ejercicio 3
testSuiteEjercicio3 = test [
    "Caso 1: Red con un usuario" ~: (cantidadDeAmigos redTest3_1 usuario2) ~?= 0,

    "Caso 2: Red con varios usuarios y usuario sin relaciones" ~: (cantidadDeAmigos redTest3_2 usuario1) ~?= 0,

    "Caso 3: Red con varios usuarios y usuario con una relación" ~: (cantidadDeAmigos redTest3_3 usuario1) ~?= 1,

    "Caso 4: Red con varios usuarios y usuario con varias relaciones" ~: (cantidadDeAmigos redTest3_4 usuario1) ~?= 2
        ]


-- Casos de Test Ejercicio 4
redTest4_1 = ([usuario1], [], [])
redTest4_2 = ([usuario1, usuario2], [relacion1_2], [])
redTest4_3 = ([usuario1, usuario2, usuario3, usuario4], [relacion1_2, relacion3_4], [])
redTest4_4 = ([usuario1, usuario2, usuario3], [relacion1_2, relacion2_3, relacion2_4], [])


-- Ejercicio 4
testSuiteEjercicio4 = test [
    "Caso 1: Red con un usuario" ~: (usuarioConMasAmigos redTest4_1) ~?= usuario1,

    "Caso 2: Red con dos usuarios con una relación entre ambos" ~: (usuarioConMasAmigos redTest4_2) ~?= usuario1,

    "Caso 3: Red con varios usuarios con misma cantidad máxima de amigos" ~: (usuarioConMasAmigos redTest4_3) ~?= usuario1,

    "Caso 4: Red con varios usuarios y usuario con la cantidad máxima de amigos" ~: (usuarioConMasAmigos redTest4_4) ~?= usuario2
        ]


-- Casos de Test Ejercicio 5
redTest5_1 = redVacia
redTest5_2 = ([usuario1, usuario2, usuario3], [], [])
redTest5_3 = ([usuario1, usuario2, usuario3, usuario4, usuario5], [relacion1_2, relacion1_3, relacion1_4, relacion2_3, relacion3_4], [])
redTest5_4 = ([usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11], [relacion1_2, relacion2_3], [])
redTest5_5 = ([usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11, usuario12], [relacion1_2, relacion1_3, relacion1_4, relacion1_5, relacion1_6, relacion1_7, relacion1_8, relacion1_9, relacion1_10, relacion1_11, relacion1_12], [])
redTest5_6 = ([usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11, usuario12], [relacion1_2, relacion1_3, relacion1_4, relacion1_5, relacion1_6, relacion1_7, relacion1_8, relacion1_9, relacion1_10, relacion1_11, relacion1_12, relacion2_3, relacion2_4, relacion2_5, relacion2_6, relacion2_7, relacion2_8, relacion2_9, relacion2_10, relacion2_11, relacion2_12], [])

-- Ejercicio 5
testSuiteEjercicio5 = test [
    "Caso 1: Red vacía" ~: (estaRobertoCarlos redTest5_1) ~?= False,

    "Caso 2: Red sin amistades" ~: (estaRobertoCarlos redTest5_2) ~?= False,

    "Caso 3: Red con 10 o menos usuarios" ~: (estaRobertoCarlos redTest5_3) ~?= False,

    "Caso 4: Red con mas de 10 usuarios y ninguno relacionado con mas de 10 usuarios" ~: (estaRobertoCarlos redTest5_4) ~?= False,

    "Caso 5: Red con mas de 10 usuarios y uno relacionado con mas de 10 usuarios" ~: (estaRobertoCarlos redTest5_5) ~?= True,

    "Caso 6: Red con mas de 10 usuarios y varios relacionados con mas de 10 usuarios" ~: (estaRobertoCarlos redTest5_6) ~?= True
        ]

--Casos de Test Ejercicio 6
redTest6_1 = ([usuario6], [], [])
redTest6_2 = ([usuario6, usuario7, usuario8], [], [publicacion6_2])
redTest6_3 = ([usuario6, usuario7, usuario8], [], [publicacion6_2])
redTest6_4 = ([usuario1, usuario3, usuario6, usuario7, usuario8], [], [publicacion6_1, publicacion6_2, publicacion8_1])
redTest6_5 = ([usuario6, usuario7, usuario8], [], [publicacion6_2, publicacion8_3, publicacion8_1])
redTest6_6 = ([usuario6, usuario7, usuario8], [], [publicacion6_2, publicacion8_1, publicacion7_4])

testSuiteEjercicio6 = test [

    "Caso 1:Red sin publicaciones" ~: (publicacionesDe redTest6_1 usuario6) ~?= [],

    "Caso 2:Red con una publicación pero no del usuario" ~: (publicacionesDe redTest6_2 usuario7) ~?= [],

    "Caso 3:Red con una publicación del usuario" ~: (publicacionesDe redTest6_3 usuario6) ~?= [publicacion6_2],

    "Caso 4:Red con varias publicaciones y algunas del usuario" ~: (publicacionesDe redTest6_4 usuario6) ~?= [publicacion6_1, publicacion6_2],

    "Caso 5:Red con varias publicaciones y ninguna del usuario" ~: (publicacionesDe redTest6_5 usuario7) ~?= [],
    
    "Caso 6:Red con varias publicaciones y una sola del usuario" ~: (publicacionesDe redTest6_6 usuario8) ~?= [publicacion8_1]   
        ]

--Casos Test Ejercico 7
redTest7_1 = ([usuario7], [], [])
redTest7_2 = ([usuario7, usuario8, usuario9], [], [publicacion8_1])
redTest7_3 = ([usuario7, usuario8, usuario9], [], [publicacion9_2])
redTest7_4 = ([usuario6, usuario7, usuario8], [], [publicacion6_2, publicacion7_4, publicacion8_1])
redTest7_5 = ([usuario6, usuario7, usuario8], [], [publicacion6_2, publicacion7_4, publicacion8_1, publicacion8_3])
redTest7_6 = ([usuario6, usuario7, usuario8], [], [publicacion8_1, publicacion8_3])


testSuiteEjercicio7 = test [

    "Caso 1:Red sin publicaciones" ~: (publicacionesDe redTest7_1 usuario7) ~?= [],

    "Caso 2:Red con una publicación con like del usuario" ~: (publicacionesQueLeGustanA redTest7_2 usuario7) ~?= [publicacion8_1],

    "Caso 3:Red con una publicación sin like del usuario" ~: (publicacionesQueLeGustanA redTest7_3 usuario7) ~?= [],

    "Caso 4:Red con varias publicaciones y una con like del usuario" ~: (publicacionesQueLeGustanA redTest6_4 usuario7) ~?= [publicacion8_1],

    "Caso 5:Red con varias publicaciones y algunas con like del usuario" ~:  (publicacionesQueLeGustanA redTest7_5 usuario7) ~?= [publicacion8_1, publicacion8_3],
    
    "Caso 6:Red con varias publicaciones todas con like del usuario" ~: (publicacionesQueLeGustanA redTest7_6 usuario7) ~?= [publicacion8_1, publicacion8_3]  
        ]


--Casos Test Ejercico 8
redTest8_1 = ([usuario7, usuario8], [], [])
redTest8_2 = ([usuario7, usuario1, usuario3], [], [publicacion7_1])
redTest8_3 = ([usuario7, usuario8], [], [publicacion7_4])
redTest8_4 = ([usuario7, usuario9, usuario10], [], [publicacion7_2])
redTest8_5 = ([usuario9, usuario8], [], [publicacion9_2])
redTest8_6 = ([usuario9, usuario6], [], [publicacion6_2, publicacion9_2, publicacion9_3])
redTest8_7 = ([usuario7, usuario9, usuario10], [], [publicacion7_2, publicacion9_2, publicacion10_2])
redTest8_8 = ([usuario7, usuario8, usuario9, usuario10], [], [publicacion9_2, publicacion10_2, publicacion10_1, publicacion7_3])

testSuiteEjercicio8 = test [

    "Caso 1: Red sin publicaciones" ~: (lesGustanLasMismasPublicaciones redTest8_1 usuario7 usuario8) ~?= True,

    "Caso 2: Red con una publicación con like de ambos usuarios" ~: (lesGustanLasMismasPublicaciones redTest8_2 usuario1 usuario3) ~?= True,

    "Caso 3: Red con una publicación sin like de ambos usuario" ~: (lesGustanLasMismasPublicaciones redTest8_3 usuario7 usuario8) ~?= True, 

    "Caso 4: Red con una publicacion con like de uno solo de los usuarios" ~: (lesGustanLasMismasPublicaciones redTest8_4 usuario7 usuario10) ~?= False,

    "Caso 5: Red con una publicacion con el mismo usuario ingresado dos veces" ~:  (lesGustanLasMismasPublicaciones redTest8_5 usuario9 usuario9) ~?= True,

    "Caso 6: Red con varias publicaciones del usuario ingresado dos veces" ~:  (lesGustanLasMismasPublicaciones redTest8_6 usuario9 usuario9) ~?= True,

    "Caso 7: Red con varias publicaciones con like de uno solo de los usuarios" ~:  (lesGustanLasMismasPublicaciones redTest8_7 usuario7 usuario10) ~?= False,

    "Caso 8: Red con varias publicaciones con likes de ambos usuarios" ~: (lesGustanLasMismasPublicaciones redTest8_8 usuario9 usuario10) ~?= True 
        ]


--Casos Test Ejercico 9
redTest9_1 = ([usuario1, usuario2, usuario4], [], [publicacion2_1])
redTest9_2 = ([usuario1, usuario2], [], [publicacion1_4])
redTest9_3 = ([usuario1, usuario2, usuario4], [], [publicacion1_1])
redTest9_4 = ([usuario1], [], [publicacion1_6, publicacion1_4])
redTest9_5 = ([usuario1, usuario2, usuario4, usuario5], [], [publicacion1_1, publicacion1_2, publicacion1_3])
redTest9_6 = ([usuario1, usuario2, usuario4], [], [publicacion1_1, publicacion1_2])

testSuiteEjercicio9 = test [
    "Caso 1: Red sin publicaciones del usuario" ~: (tieneUnSeguidorFiel redTest9_1 usuario1) ~?= False,

    "Caso 2: Red con una publicacion del usuario sin likes" ~: (tieneUnSeguidorFiel redTest9_2 usuario1) ~?= False,

    "Caso 3: Red con una publicacion del usuario con likes" ~: (tieneUnSeguidorFiel redTest9_3 usuario1) ~?= True,

    "Caso 4: Red con varias publicaciones del usuario sin likes" ~: (tieneUnSeguidorFiel redTest9_4 usuario1) ~?= False,

    "Caso 5: Red con varias publicaciones del usuario con likes repetidos en algunas" ~: (tieneUnSeguidorFiel redTest9_5 usuario1) ~?= False,

    "Caso 6: Red con varias publicaciones del usuario con likes repetidos en todas" ~: (tieneUnSeguidorFiel redTest9_6 usuario1) ~?= True
        ]

--Casos Test Ejercico 10
redTest10_1 = ([usuario3, usuario4],[],[])
redTest10_2 = ([usuario3, usuario4],[relacion3_4],[])
redTest10_3 = ([usuario3, usuario4],[relacion3_4],[])
redTest10_4 = ([usuario1, usuario2, usuario3, usuario4],[],[])
redTest10_5 = ([usuario1, usuario2, usuario3, usuario4, usuario5],[relacion2_3],[])
redTest10_6 = ([usuario1, usuario2, usuario3, usuario4, usuario5],[relacion2_3, relacion3_4],[])

testSuiteEjercicio10 = test [
    "Caso 1: Red con dos usuarios consecutivos y ordenados, pero sin relacionar" ~: (existeSecuenciaDeAmigos redTest10_1 usuario3 usuario4) ~?= False,

    "Caso 2: Red con dos usuarios consecutivos y relacionados, pero invertidos" ~: (existeSecuenciaDeAmigos redTest10_2 usuario4 usuario3) ~?= False,

    "Caso 3: Red con dos usuarios consecutivos, relacionados y ordenados" ~: (existeSecuenciaDeAmigos redTest10_3 usuario3 usuario4) ~?= True,

    "Caso 4: Red con dos usuarios no consecutivos, pero en orden invertido" ~: (existeSecuenciaDeAmigos redTest10_4 usuario3 usuario1) ~?= False,

    "Caso 5: Red con dos usuarios no consecutivos, ordenados y entre ellos no hay cadenaDeAmigos" ~: (existeSecuenciaDeAmigos redTest10_5 usuario2 usuario4) ~?= False,

    "Caso 6: Red con dos usuarios no consecutivos, ordenados y entre ellos hay cadenaDeAmigos" ~: (existeSecuenciaDeAmigos redTest10_6 usuario2 usuario4) ~?= True
        ]

expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)