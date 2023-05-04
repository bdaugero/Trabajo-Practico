
-- Ejemplos

usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")

relacion1_2 = (usuario1, usuario2)
relacion2_1 = (usuario2, usuario1)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario3, usuario4)

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
publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])


usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)


-- Intento de Ejercicio 1

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])


idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

--preludios de Usuario

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

todosDistintos :: Eq a => [a] -> Bool
todosDistintos [] = True
todosDistintos (x:xs) = not (pertenece x xs) && todosDistintos xs


largo :: (Eq t) => [t] -> Int
largo [] = 0
largo (x:xs) = largo xs + 1


pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece n (x:xs) | n == x = True
                   | otherwise = pertenece n (xs)

listaPerteneceLista :: (Eq t) => [t] -> [t] -> Bool
listaPerteneceLista [] ls = True
listaPerteneceLista (x:xs) ls = (pertenece x ls) && listaPerteneceLista xs ls

mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos ls1 ls2 = listaPerteneceLista ls1 ls2 && listaPerteneceLista ls2 ls1

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

usuarioValido :: Usuario -> Bool
usuarioValido u = (idDeUsuario u) > 0 &&  (largo (nombreDeUsuario u) > 0)

-- Funcion que devuelve una lista con los id del Usuario
soloElIDUsuario :: [Usuario] -> [Integer]
soloElIDUsuario [(x)] = [fst(x)]
soloElIDUsuario (x:xs) = fst(x) : soloElIDUsuario xs


noHayIdRepetidos :: [Usuario] -> Bool
noHayIdRepetidos us = todosDistintos (soloElIDUsuario (us))

usuariosValidos :: [Usuario] -> Bool
usuariosValidos [] = True
usuariosValidos (x:xs) | usuarioValido (x) == (noHayIdRepetidos (x:xs)) = usuariosValidos xs  
                       | otherwise = False

--preludios de Relaciones

perteneceLaRelacionALaListaDeUsuarios :: Relacion -> [Usuario] -> Bool
perteneceLaRelacionALaListaDeUsuarios relacion (y:ys) = pertenece (fst relacion) (y:ys) && pertenece (snd relacion) (y:ys)

estanLosUsuariosDeLaRelacionEnLaListaDeUsuarios ::  [Relacion] -> [Usuario] -> Bool
estanLosUsuariosDeLaRelacionEnLaListaDeUsuarios [] _ = True
estanLosUsuariosDeLaRelacionEnLaListaDeUsuarios (x:xs) (y:ys) | perteneceLaRelacionALaListaDeUsuarios x (y:ys) = estanLosUsuariosDeLaRelacionEnLaListaDeUsuarios xs (y:ys)
                                                              | otherwise = False

noHayRelacionReflexiva :: [Relacion] -> Bool
noHayRelacionReflexiva [] = True
noHayRelacionReflexiva (x:xs) | fst x /= snd x = noHayRelacionReflexiva (xs)
                              | otherwise = False

usuariosDeRelacionValidos :: [Relacion] -> [Usuario] -> Bool
usuariosDeRelacionValidos x y =  noHayRelacionReflexiva x && estanLosUsuariosDeLaRelacionEnLaListaDeUsuarios x y

darVueltaRelacion :: Relacion -> Relacion
darVueltaRelacion (x, y) = (y, x)

hayRelacionesSimetricas :: [Relacion] -> Bool
hayRelacionesSimetricas (x:xs) = pertenece (darVueltaRelacion x) xs 

relacionesAsimetricas :: [Relacion] -> Bool
relacionesAsimetricas x = not (hayRelacionesSimetricas x)

soloElIdRelaciones :: [Relacion] -> [(Integer, Integer)]
soloElIdRelaciones [] = []
soloElIdRelaciones (x:xs) = (fst (fst x), fst (snd x)) : [] ++ soloElIdRelaciones xs

noHayRelacionesRepetidas :: [Relacion] -> Bool
noHayRelacionesRepetidas (x:xs) = todosDistintos (soloElIdRelaciones (x:xs))

relacionesValidas :: [Usuario] -> [Relacion] -> Bool
relacionesValidas x y = (usuariosDeRelacionValidos y x) && (relacionesAsimetricas y) && (noHayRelacionesRepetidas y)

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

meter2UsuariosEnDupla :: Usuario -> Usuario -> (Usuario, Usuario)
meter2UsuariosEnDupla u1 u2 = (u1, u2)

relacionadosDirecto :: Usuario -> Usuario -> RedSocial -> Bool
relacionadosDirecto u1 u2 x = pertenece (u1, u2) (relaciones x) || pertenece (u2, u1) (relaciones x)
 
cadenaDeAmigos :: [Usuario] -> RedSocial -> Bool
cadenaDeAmigos (a:b:[]) y = relacionadosDirecto a b y  
cadenaDeAmigos (x:xs) y | relacionadosDirecto x (head xs) y = cadenaDeAmigos (xs) y
                        | otherwise = False

-- Funcion del archivo iap1-tp --
usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

--preludios de Publicacion

publicacionesValidas :: [Usuario] -> [Publicacion] -> Bool
publicacionesValidas us ps = usuariosDePublicacionSonUsuariosDeRed us ps && noHayPublicacionesRepetidas ps

usuariosDePublicacionSonUsuariosDeRed :: [Usuario] -> [Publicacion] -> Bool
usuariosDePublicacionSonUsuariosDeRed _ [] = True
usuariosDePublicacionSonUsuariosDeRed x (y:ys) = pertenece (usuarioDePublicacion y) x && usuariosDePublicacionSonUsuariosDeRed x ys

noHayPublicacionesRepetidas :: [Publicacion] -> Bool
noHayPublicacionesRepetidas (a:[]) = True
noHayPublicacionesRepetidas (x:xs) = publicacionesDistintas x (head xs) && noHayPublicacionesRepetidas xs

publicacionesDistintas :: Publicacion -> Publicacion -> Bool
publicacionesDistintas x y | idDeUsuario (usuarioDePublicacion x) /= idDeUsuario (usuarioDePublicacion y) = True
                           | textoDePublicacion x /= textoDePublicacion y = True
                           | otherwise = False

textoDePublicacion :: Publicacion -> String
textoDePublicacion (_,tx,_) = tx

sonDeLaRed :: RedSocial -> [Usuario] -> Bool
sonDeLaRed x [] = True
sonDeLaRed x (y:ys) | pertenece y (usuarios x) = sonDeLaRed x (ys)
                    | otherwise = False
                               



































