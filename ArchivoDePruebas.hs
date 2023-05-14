module ArchivoDePruebas where

-- Nombre de Grupo: Algorritmo
-- Integrante 1: Bautista D'Augero, bdaugero@gmail.com, 100/21
-- Integrante 2: Nombre Apellido, email, LU
-- Integrante 3: Nombre Apellido, email, LU
-- Integrante 4: Nombre Apellido, email, LU

--Definiciones de tipos
type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

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



-- Ejercicio 1

{-
nombresDeUsuarios de una red social toma los usuarios de la red y devuelve únicamente el conjunto de nombres, es decir, 
elimina los repetidos de la lista que solo contiene a los nombres obtenida con soloNombresUsuarios.
-}

nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios ([],[],[]) = []
nombresDeUsuarios red = proyectarNombres (usuarios red)


proyectarNombres :: [Usuario] -> [String]
proyectarNombres us = eliminarRepetidos (soloNombresUsuarios us)

-- Funciones auxiliares para proyectarNombres

{-
soloNombresUsuarios va tomando la segunda coordenada de cada primer elemento de la lista de usuarios, es decir
el nombre, haciendo recursión con el resto de la lista hasta llegar al último usuario y así obtener la lista de nombres
(admite repetidos).
-}

soloNombresUsuarios :: [Usuario] -> [String]
soloNombresUsuarios [u] = snd u
soloNombresUsuarios (u:us) = snd u : soloNombresUsuarios us


eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) = x : eliminarRepetidos (quitarTodos x xs)


quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos _ [] = []
quitarTodos x ls | quitar x ls == ls = ls
                 | otherwise = quitarTodos x (quitar x ls)


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
amigosDe red u = eliminarRepetidos (listaAmistades (relaciones red) u)

{-
listaAmistades chequea si en la primera relación de la lista, el usuario ingresado es igual al usuario de la primera o
segunda tupla de la relación (en cuyo caso, son amigos) y agrega el segundo o el primer usuario respectivamente, luego
recursa sobre el tail hasta llegar a la lista vacía.
-}

listaAmistades :: [Relacion] -> Usuario -> [Usuario]
listaAmistades [] u = []
listaAmistades (r:rs) u | fst r == u = snd r : listaAmistades rs u
                        | snd r == u = fst r : listaAmistades rs u
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

{-
usuarioConMasAmigos es la evaluación de usuarioConMasAmigosAux en la lista de usuarios de la red.
-}

usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos red = usuarioConNAmigos red (usuarios red)

-- Funciones auxiliares para usuarioConMasAmigos

{-
usuarioConNamigos va recorriendo la lista de usuarios de la red hasta encontrar un usuario que coincide con la cantidad
de amigos máxima de la red.
-}

-- ¿les parece cambiar el nombre de UsuarioConNAmigos por usuarioConMasAmigosAux? porque devuelve el usuario con mas amigos
--lo de N creo que confunde porque ademas no está n en la función... despues díganme

usuarioConNAmigos :: RedSocial -> [Usuario] -> Usuario
usuarioConNAmigos red [u] = u
usuarioConNAmigos red (u:us) | mayorCantidadDeAmigos red == cantidadDeAmigos red u = u
                             | otherwise = usuarioConNAmigos red us


mayorCantidadDeAmigos :: RedSocial -> Integer
mayorCantidadDeAmigos red = maximo (cantidadesDeAmigos red (usuarios red))

{-
cantidadesDeAmigos va creando una lista con la cantidad de amigos que tiene cada usuario de la lista de usuarios.
-}

cantidadesDeAmigos :: RedSocial -> [Usuario] -> [Integer]
cantidadesDeAmigos red [] = []
cantidadesDeAmigos red (u:us) = cantidadDeAmigos red u : cantidadesDeAmigos red us


maximo :: [Integer] -> Integer
maximo (x:xs) | xs == [] = x
              | x >= head xs = maximo (x : tail xs)
              | x < head xs = maximo xs


-- Ejercicio 5

{-
estaRobertoCarlos chequea si existe algún usuario de la red que tenga más de 10 amigos. 
-}

estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos ([],[],[]) = False
estaRobertoCarlos red = (mayorCantidadDeAmigos red) > 10


-- Ejercicio 6

publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe ([],[],[]) _ = []
publicacionesDe red u = eliminarRepetidos (publicacionesDeAux (publicaciones red) u)

{-
publicacionesDeAux va creando una lista con las publicaciones del usuario de entrada chequeando en cada publicación de la
lista si ese usuario es igual al usuario de la publicación (en cuyo caso, el la publicó). Si no lo es, no la agrega y 
continúa con el siguiente. 
-}

publicacionesDeAux :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesDeAux [] u = []
publicacionesDeAux (p:ps) u | u == usuarioDePublicacion p = p : publicacionesDeAux ps u 
                            | otherwise = publicacionesDeAux ps u 


-- Ejercicio 7

publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA ([],[],[]) _ = []
publicacionesQueLeGustanA red u = eliminarRepetidos (publicacionesQueLeGustanAaux (publicaciones red) u)

{-
publicacionesQueLeGustanAaux se pregunta si el usuario ingresado es un elemento de la lista de likes de cada publicación 
de la lista, si lo es agrega la publicación a una lista y sigue con el resto. Si no lo es, entonces no es una publicación
que le guste (por lo tanto no la agrega) y se pregunta por el siguiente.  
-}

publicacionesQueLeGustanAaux :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesQueLeGustanAaux [] u = []
publicacionesQueLeGustanAaux (p:ps) u |pertenece u (likesDePublicacion p) =  p : publicacionesQueLeGustanAaux ps u 
                                      | otherwise = publicacionesQueLeGustanAaux ps u 


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



-- Predicados auxiliares

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece n (x:xs) | n == x = True
                   | otherwise = pertenece n xs


mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos ls1 ls2 = listaPerteneceLista ls1 ls2 && listaPerteneceLista ls2 ls1


--El primer elemento de una relacion es el segundo de la relacion previa 
cadenaDeAmigos :: [Usuario] -> RedSocial -> Bool
cadenaDeAmigos (a:b:[]) red = relacionadosDirecto a b red  
cadenaDeAmigos (u:us) red | relacionadosDirecto u (head us) red = cadenaDeAmigos us red
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