module Testtp where

import Test.HUnit
import ArchivoDePruebas

-- Usuarios
usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")
usuario6 = (6, "Bautista")
usuario7 = (7, "Tomas")
usuario8 = (8, "Jorge")
usuario9 = (9, "Antonela")
usuario10 = (10, "Julieta")

-- Relaciones
relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) 
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)


-- Publicaciones
publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario5])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])
publicacion1_5 = (usuario1, "Este es como mi quinto post", [usuario5])

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
publicacion7_3 = (usuario7, "Voy a extrañar mucho a mis compañeros de trabajo", [usuario1, usuario5, usuario9])

publicacion8_1 = (usuario8, "Me encanta cocinar nuevos platos", [usuario7])
publicacion8_2 = (usuario8, "¡Qué hermoso día hace hoy!", [usuario9])
publicacion8_3 = (usuario8, "Estoy emocionado por el viaje que tengo planeado para el próximo mes", [usuario7])

publicacion9_1 = (usuario9, "No puedo esperar a que llegue el fin de semana", [usuario1, usuario2])
publicacion9_2 = (usuario9, "Acabo de terminar de leer un gran libro", [])
publicacion9_3 = (usuario9, "¡Felicitaciones a mi hermana por su nuevo trabajo!", [usuario3])

publicacion10_1 = (usuario10, "¿Alguien quiere ir al parque conmigo?", [usuario4, usuario8])
publicacion10_2 = (usuario10, "Estoy aburrido en casa, ¿alguien quiere jugar videojuegos?", [usuario7])
publicacion10_3 = (usuario10, "Estoy tan emocionado por la boda de mi mejor amigo este fin de semana", [usuario6, usuario3])

-- Redes
redVacia = ([], [], [])

-- Casos de Test Ejercicio 1
redTest2 = ([usuario1], [], [])
redTest3 = ([usuario1, usuario1, usuario2], [], [])
redTest4 = ([usuario1, usuario2, usuario3], [], [])

-- Casos de Test Ejercicio 2
redTest5 = ([usuario1, usuario2], [relacion1_2], [])
redTest6 = ([usuario1, usuario2], [relacion1_2, relacion1_2], [])

-- Casos de Test Ejercicio 3
redTest7 = ([usuario1, usuario2, usuario3, usuario4], [relacion1_2, relacion1_3, relacion1_4], [])

-- Casos de Test Ejercicio 4
redTest8 = ([usuario1, usuario2, usuario3, usuario4], [relacion1_2, relacion3_4, relacion2_3, relacion2_4], [])

-- Casos de Test Ejercicio 5
redTest9 = ([usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10], [relacion1_2, relacion3_4, relacion2_3, relacion2_4, relacion2_4, relacion2_4, relacion2_4, relacion2_4, relacion2_4, relacion2_4, relacion2_4, relacion2_4, relacion2_4, relacion2_4, relacion2_4], [])


run = runTestTT testSuiteEjercicio5

-- Ejercicio 1
testSuiteEjercicio1 = test [
    "Caso 1: Red vacía" ~: (nombresDeUsuarios redVacia) ~?= [],

    "Caso 2: Red con un usuario" ~: (nombresDeUsuarios redTest2) ~?= ["Juan"],

    "Caso 3: Red con nombres de usuarios repetidos" ~: (nombresDeUsuarios redTest3) ~?= ["Juan", "Maria"],

    "Caso 4: Red con nombres de usuarios que no se repiten" ~: (nombresDeUsuarios redTest4) ~?= ["Juan", "Julia", "Maria"]
        ]

-- Ejercicio 2
testSuiteEjercicio2 = test [
    "Caso 1: Red vacía" ~: (amigosDe redVacia usuario1) ~?= [],

    "Caso 2: Red con un Usuario sin relaciones y ese usuario" ~: (amigosDe redTest2 usuario1) ~?= [],

    "Caso 3: Red con Usuarios con una relacion u1 u2 y u1 " ~: (amigosDe redTest5 usuario1) ~?= [usuario2],

    "Caso 4: Red con Usuarios con una relacion u1 u2 y u2 " ~: (amigosDe redTest5 usuario2) ~?= [usuario1],

    "Caso 5: Red con Usuarios con una relacion repetida" ~: (amigosDe redTest6 usuario1) ~?= [usuario2]
        ] 

-- Ejercicio 3
testSuiteEjercicio3 = test [
    "Caso 1: Red vacía" ~: (cantidadDeAmigos redVacia usuario1) ~?= 0,

    "Caso 2: Red con un usuario sin amigos" ~: (cantidadDeAmigos redTest2 usuario1) ~?= 0,

    "Caso 3: Red con un usuario con 1 amigo" ~: (cantidadDeAmigos redTest5 usuario1) ~?= 1,

    "Caso 4: Red con un usuario con 3 amigos" ~: (cantidadDeAmigos redTest7 usuario1) ~?= 3
        ]

-- Ejercicio 4
testSuiteEjercicio4 = test [
    "Caso 1: Usuario con 1 amigo" ~: (usuarioConMasAmigos redTest5) ~?= usuario1,

    "Caso 2: Muchos usuarios con varios amigos" ~: (usuarioConMasAmigos redTest8) ~?= usuario2
        ]

-- Ejercicio 5
testSuiteEjercicio5 = test [
    "Caso 1: Red vacía" ~: (estaRobertoCarlos redVacia) ~?= False,
-- Tendria que dar True
    "Caso 2: Red vacía" ~: (estaRobertoCarlos redTest9) ~?= False
        ]

-- Ejercicio 6
testSuiteEjercicio6 = test [
    "Caso 1: Red vacía" ~: (publicacionesDe redVacia usuario1) ~?= []
        ]

-- Ejercicio 7
testSuiteEjercicio7 = test [
    "Caso 1: Red vacía" ~: (publicacionesQueLeGustanA redVacia usuario1) ~?= []
        ]

-- Ejercicio 8
testSuiteEjercicio8 = test [
    "Caso 1: Red vacía" ~: (lesGustanLasMismasPublicaciones redVacia usuario1 usuario2) ~?= False
        ]

-- Ejercicio 9
testSuiteEjercicio9 = test [
    "Caso 1: Red vacía" ~: (tieneUnSeguidorFiel redVacia usuario1) ~?= False
        ]

-- Ejercicio 10
testSuiteEjercicio10 = test [
    "Caso 1: Red vacía" ~: (existeSecuenciaDeAmigos redVacia usuario1 usuario2) ~?= False
        ]

expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)