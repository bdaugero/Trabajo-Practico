-- Ejemplos

usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Jose")

relacion1_2 = (usuario1, usuario2)
relacion2_1 = (usuario2, usuario1)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario3, usuario4)
relacion2_5 = (usuario2, usuario5)

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

usuariosC = [usuario1, usuario2, usuario3, usuario5]
relacionesC = [relacion1_2, relacion2_3, relacion1_2, relacion1_2, relacion1_2, relacion1_2, relacion1_2, relacion1_2, relacion1_2, relacion1_2,relacion1_2, relacion1_2]
publicacionesC = [publicacion3_3]
redC = (usuariosC, relacionesC, publicacionesC)

redVacia = ([],[],[])

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])



-- Ejercicio 1

{-
nombresDeUsuarios de una red social toma los usuarios de la red y devuelve únicamente el conjunto de nombres, es decir, 
elimina los repetidos de esa lista que solo contiene a los nombres obtenida con soloNombresUsuarios.
-}

nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios ([],[],[]) = []
nombresDeUsuarios red = proyectarNombres (usuarios red)

proyectarNombres :: [Usuario] -> [String]
proyectarNombres us = eliminarRepetidos (soloNombresUsuarios us)

-- Funciones auxiliares para proyectarNombres

{-
soloNombresUsuarios va tomando la segunda coordenada de cada primer elemento de la lista de usuarios, es decir
el nombre, haciendo recursión con el resto de la lista hasta llegar al último usuario y así obtener la lista de nombres.
-}

soloNombresUsuarios :: [Usuario] -> [String]
soloNombresUsuarios [u] = [snd u]
soloNombresUsuarios (u:us) = snd(u) : soloNombresUsuarios us

{-

-}

eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) = x : eliminarRepetidos (quitarTodos x xs)


quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos _ [] = []
quitarTodos x ls | quitar x ls == ls = ls
                 | otherwise = quitarTodos x (quitar x ls)

{-
quitar es una función que quita la primera aparición de un elemento en una lista de elementos de su mismo tipo, entonces si
ese elemento no es igual al head y pertenece a la lista, lo agrega y recursa sobre el tail de la lista.
-}

quitar :: (Eq t) => t -> [t] -> [t]
quitar n (x:xs) | not (pertenece n (x:xs)) = (x:xs)
                | n == x = xs
                | otherwise = x : quitar n xs


-- Ejercicio 2

{-
amigosDe toma como función auxiliar listaAmistades, a la cual se le pasa como primer parámetro la lista de relaciones
de la red social.
-}


amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe ([],[],[]) _ = []
amigosDe red u = listaAmistades (relaciones red) u

{-
listaAmistades chequea si en la primera relación de la lista, el usuario ingresado es igual al usuario de la primera o
segunda tupla de la relación (en cuyo caso, son amigos) y agrega el segundo o el primer usuario respectivamente, luego
recursa sobre el tail hasta llegar a la lista vacía.
-}

listaAmistades :: [Relacion] -> Usuario -> [Usuario]
listaAmistades [] u = []
listaAmistades (r:rs) u | fst r == u = (snd r) : listaAmistades rs u
                        | snd r == u = (fst r) : listaAmistades rs u
                        | otherwise = listaAmistades rs u


-- Ejercicio 3

{-
cantidadDeAmigos dada una red y un usuario, devuelve la cantidad de amigos de dicho usuario entendiendo que dicha cantidad
será igual a la longitud que tiene la lista de amigos del usuario (que es otra forma de contarlos).
-}

cantidadDeAmigos :: RedSocial -> Usuario -> Integer
cantidadDeAmigos ([],[],[]) _ = 0
cantidadDeAmigos red u = largo (amigosDe red u)


-- Ejercicio 4
-- El caso de la red vacia no se si tiene sentido en este ejercicio ya que si la red es vacia no existe un usuario con mas amigos
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos red = usuarioConNAmigos red (usuarios red)

-- Funciones auxiliares para usuarioConMasAmigos

usuarioConNAmigos :: RedSocial -> [Usuario] -> Usuario
usuarioConNAmigos red [u] = u
usuarioConNAmigos red (u:us) | mayorCantidadDeAmigos red == cantidadDeAmigos red u = u
                           | otherwise = usuarioConNAmigos red us

mayorCantidadDeAmigos :: RedSocial -> Integer
mayorCantidadDeAmigos red = maximo (cantidadesDeAmigos red (usuarios red))


cantidadesDeAmigos :: RedSocial -> [Usuario] -> [Integer]
cantidadesDeAmigos red [] = []
cantidadesDeAmigos red (u:us) = cantidadDeAmigos red u : cantidadesDeAmigos red us

{-
maximo de una lista de enteros va comparando los elementos de la lista uno a uno, descartando al menor de ellos y
recursando sobre el tail de la lista hasta que queda un elemento (cuyo tail es la lista vacía) y devuelve dicho elemento, 
que será el máximo.
-}

maximo :: [Integer] -> Integer
maximo (x:xs) | xs == [] = x
              | x >= head xs = maximo (x : (tail xs))
              | x < head xs = maximo xs


-- Ejercicio 5

estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos ([],[],[]) = False
estaRobertoCarlos red = (mayorCantidadDeAmigos red) > 10


-- Ejercicio 6

publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe ([],[],[]) _ = []
publicacionesDe red u = (publicacionesDeAux (publicaciones red) u)

publicacionesDeAux :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesDeAux [] u = []
publicacionesDeAux (p:ps) u | u == (usuarioDePublicacion p) =  p : publicacionesDeAux (ps) u 
                            | otherwise = publicacionesDeAux (ps) u 

 

-- Ejercicio 7

publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA ([],[],[]) _ = []
publicacionesQueLeGustanA red u = publicacionesQueLeGustanAAux (publicaciones red) u

publicacionesQueLeGustanAAux :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesQueLeGustanAAux [] u = []
publicacionesQueLeGustanAAux (p:ps) u |pertenece u (likesDePublicacion p) =  p : publicacionesQueLeGustanAAux (ps) u 
                                      | otherwise = publicacionesQueLeGustanAAux (ps) u 


-- Ejercicio 8

lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones ([],[],[]) _ _ = False
lesGustanLasMismasPublicaciones red u1 u2 = mismosElementos (publicacionesQueLeGustanA red u1) (publicacionesQueLeGustanA red u2)


-- Ejercicio 9

tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel ([],[],[]) _ = False
tieneUnSeguidorFiel red u = tieneUnSeguidorFielAux (usuarios red) (listasDeLikesDeUsuario (publicacionesDe red u))

tieneUnSeguidorFielAux :: [Usuario] -> [[Usuario]] -> Bool
tieneUnSeguidorFielAux [] ls = False
tieneUnSeguidorFielAux (u:us) ls | esSeguidorFiel u ls = True
                                 | otherwise = tieneUnSeguidorFielAux us ls

esSeguidorFiel :: Usuario -> [[Usuario]] -> Bool
esSeguidorFiel u [] = True
esSeguidorFiel u (l:ls) | pertenece u l = esSeguidorFiel u ls
                        | otherwise = False

listasDeLikesDeUsuario :: [Publicacion] -> [[Usuario]]
listasDeLikesDeUsuario [] = []
listasDeLikesDeUsuario (p:ps) = likesDePublicacion p : listasDeLikesDeUsuario ps


-- Ejercicio 10

existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos ([],[],[]) _ _ = False
existeSecuenciaDeAmigos red u1 u2 = u1 /= u2 && empiezaCon u1 us && terminaCon u2 us && cadenaDeAmigos us red
    where us = usuarios red


-- Funciones básicas

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us


-- Predicados auxiliares

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece n (x:xs) | n == x = True
                   | otherwise = pertenece n (xs)


mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos ls1 ls2 = listaPerteneceLista ls1 ls2 && listaPerteneceLista ls2 ls1


--El primer elemento de una relacion es el segundo de la relacion previa 
cadenaDeAmigos :: [Usuario] -> RedSocial -> Bool
cadenaDeAmigos (a:b:[]) red = relacionadosDirecto a b red  
cadenaDeAmigos (u:us) red | relacionadosDirecto u (head us) red = cadenaDeAmigos (us) red
                          | otherwise = False


relacionadosDirecto :: Usuario -> Usuario -> RedSocial -> Bool
relacionadosDirecto u1 u2 red = pertenece (u1, u2) (relaciones red) || pertenece (u2, u1) (relaciones red)


empiezaCon :: (Eq t) => t -> [t] -> Bool
empiezaCon x [] = False
empiezaCon x ls = x == head ls


terminaCon :: (Eq t) => t -> [t] -> Bool
terminaCon x [] = False
terminaCon x ls = x == ultimo ls

ultimo :: [t] -> t
ultimo [a] = a
ultimo (x:xs) = ultimo xs



--Los siguientes predicados no están siendo utilizados por lo cual, no los necesitamos (consultar)
-- Preludios de Red Social

redSocialValida :: RedSocial -> Bool
redSocialValida red = usuariosValidos (usuarios red) && relacionesValidas (usuarios red) (relaciones red) && publicacionesValidas (usuarios red) (publicaciones red)


sonDeLaRed :: RedSocial -> [Usuario] -> Bool
sonDeLaRed red [] = True
sonDeLaRed red (u:us) | pertenece u (usuarios red) = sonDeLaRed red us
                      | otherwise = False



-- Preludios de Usuario

usuariosValidos :: [Usuario] -> Bool
usuariosValidos [] = True
usuariosValidos (u:us) | usuarioValido u == noHayIdRepetidos (u:us) = usuariosValidos us  
                       | otherwise = False


usuarioValido :: Usuario -> Bool
usuarioValido u = (idDeUsuario u) > 0 &&  (largo (nombreDeUsuario u) > 0)

largo :: (Eq t) => [t] -> Integer
largo [] = 0
largo (x:xs) = largo xs + 1


noHayIdRepetidos :: [Usuario] -> Bool
noHayIdRepetidos us = todosDistintos (soloIdsUsuarios us)


--Función que devuelve una lista con los id del Usuario
soloIdsUsuarios :: [Usuario] -> [Integer]
soloIdsUsuarios [u] = [fst u]
soloIdsUsuarios (u:us) = fst u : soloIdsUsuarios us


todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos [] = True
todosDistintos (x:xs) = not (pertenece x xs) && todosDistintos xs



-- Preludios de Relaciones

relacionesValidas :: [Usuario] -> [Relacion] -> Bool
relacionesValidas us rs = (usuariosDeRelacionValidos rs us) && (relacionesAsimetricas rs) && (noHayRelacionesRepetidas rs)


usuariosDeRelacionValidos :: [Relacion] -> [Usuario] -> Bool
usuariosDeRelacionValidos rs us =  noHayRelacionReflexiva rs && estanLosUsuariosDeLaRelacionEnLaListaDeUsuarios rs us

noHayRelacionReflexiva :: [Relacion] -> Bool
noHayRelacionReflexiva [] = True
noHayRelacionReflexiva (r:rs) | fst r /= snd r = noHayRelacionReflexiva (rs)
                              | otherwise = False

estanLosUsuariosDeLaRelacionEnLaListaDeUsuarios ::  [Relacion] -> [Usuario] -> Bool
estanLosUsuariosDeLaRelacionEnLaListaDeUsuarios [] _ = True
estanLosUsuariosDeLaRelacionEnLaListaDeUsuarios (r:rs) (u:us) | perteneceLaRelacionALaListaDeUsuarios r (u:us) = estanLosUsuariosDeLaRelacionEnLaListaDeUsuarios rs (u:us)
                                                              | otherwise = False

perteneceLaRelacionALaListaDeUsuarios :: Relacion -> [Usuario] -> Bool
perteneceLaRelacionALaListaDeUsuarios r (u:us) = pertenece (fst r) (u:us) && pertenece (snd r) (u:us)


relacionesAsimetricas :: [Relacion] -> Bool
relacionesAsimetricas rs = not (hayRelacionesSimetricas rs)

hayRelacionesSimetricas :: [Relacion] -> Bool
hayRelacionesSimetricas (r:rs) = pertenece (darVueltaRelacion r) rs

darVueltaRelacion :: Relacion -> Relacion
darVueltaRelacion (r1, r2) = (r2, r1)


noHayRelacionesRepetidas :: [Relacion] -> Bool
noHayRelacionesRepetidas (r:rs) = todosDistintos (soloElIdRelaciones (r:rs))

soloElIdRelaciones :: [Relacion] -> [(Integer, Integer)]
soloElIdRelaciones [] = []
soloElIdRelaciones (r:rs) = (fst (fst r), fst (snd r)) : [] ++ soloElIdRelaciones rs



-- Preludios de Publicacion

publicacionesValidas :: [Usuario] -> [Publicacion] -> Bool
publicacionesValidas us ps = usuariosDePublicacionSonUsuariosDeRed us ps && noHayPublicacionesRepetidas ps && usuarioDeLikeDePublicacionesSonUsuariosDeRed us ps


usuariosDePublicacionSonUsuariosDeRed :: [Usuario] -> [Publicacion] -> Bool
usuariosDePublicacionSonUsuariosDeRed _ [] = True
usuariosDePublicacionSonUsuariosDeRed us (p:ps) = pertenece (usuarioDePublicacion p) us && usuariosDePublicacionSonUsuariosDeRed us ps


noHayPublicacionesRepetidas :: [Publicacion] -> Bool
noHayPublicacionesRepetidas (a:[]) = True
noHayPublicacionesRepetidas (p:ps) = publicacionesDistintas p (head ps) && noHayPublicacionesRepetidas ps


publicacionesDistintas :: Publicacion -> Publicacion -> Bool
publicacionesDistintas p1 p2 | idDeUsuario (usuarioDePublicacion p1) /= idDeUsuario (usuarioDePublicacion p2) = True
                             | textoDePublicacion p1 /= textoDePublicacion p2 = True
                             | otherwise = False

textoDePublicacion :: Publicacion -> String
textoDePublicacion (_,tx,_) = tx


usuarioDeLikeDePublicacionesSonUsuariosDeRed :: [Usuario] -> [Publicacion] -> Bool
usuarioDeLikeDePublicacionesSonUsuariosDeRed us [] = True
usuarioDeLikeDePublicacionesSonUsuariosDeRed us (p:ps) | listaPerteneceLista (likesDePublicacion p) us = usuarioDeLikeDePublicacionesSonUsuariosDeRed us ps
                                                       | otherwise = False

listaPerteneceLista :: (Eq t) => [t] -> [t] -> Bool
listaPerteneceLista [] ls = True
listaPerteneceLista (x:xs) ls = (pertenece x ls) && listaPerteneceLista xs ls