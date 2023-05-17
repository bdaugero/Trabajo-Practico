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

-- soloNombresUsuarios toma una lista de usuarios y devuelve una lista con los nombres de esos usuarios (admite repetidos).

soloNombresUsuarios :: [Usuario] -> [String]
soloNombresUsuarios [u] = [snd u]
soloNombresUsuarios (u:us) = snd u : soloNombresUsuarios us


-- Ejercicio 2

-- amigosDe llama a la función auxiliar listaAmistades, evaluándola en la lista de relaciones de la red social.

amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe red u = eliminarRepetidos (listaAmistades (relaciones red) u)
--creo que hay que sacar eliminarRepetidos porque una red valida tiene relaciones validas y relaciones validas no admite
--relaciones repetidas.

{-
listaAmistades dada una lista de relaciones y un usuario, devuelve una lista con los amigos del usuario buscándolos en
cada relación de la lista.
-}

listaAmistades :: [Relacion] -> Usuario -> [Usuario]
listaAmistades [] u = []
listaAmistades (r:rs) u | fst r == u = snd r : listaAmistades rs u
                        | snd r == u = fst r : listaAmistades rs u
                        | otherwise = listaAmistades rs u


-- Ejercicio 3

-- cantidadDeAmigos dado un usuario y una red, devuelve la cantidad de elementos de la lista de amigos del usuario.

cantidadDeAmigos :: RedSocial -> Usuario -> Integer
cantidadDeAmigos red u = largo (amigosDe red u)


-- Ejercicio 4

-- usuarioConMasAmigos es la evaluación de usuarioConMasAmigosAux en la lista de usuarios de la red.

usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos red = usuarioConMasAmigosAux red (usuarios red)

{-
usuarioConMasAmigosAux va recorriendo la lista de usuarios de la red hasta encontrar un usuario que coincide con la cantidad
máxima de amigos de la red.
-}

usuarioConMasAmigosAux :: RedSocial -> [Usuario] -> Usuario
usuarioConMasAmigosAux red [u] = u
usuarioConMasAmigosAux red (u:us) | mayorCantidadDeAmigos red == cantidadDeAmigos red u = u
                             | otherwise = usuarioConMasAmigosAux red us

-- mayorCantidadDeAmigos dada una red social devuelve la cantidad máxima de usuarios que existe en la red.

mayorCantidadDeAmigos :: RedSocial -> Integer
mayorCantidadDeAmigos red = maximo (cantidadesDeAmigos red (usuarios red))

{-
cantidadesDeAmigos dada una red social y una lista de usuarios de la red, devuelve una lista con la cantidad de amigos
que tiene cada usuario.
-}

cantidadesDeAmigos :: RedSocial -> [Usuario] -> [Integer]
cantidadesDeAmigos red [] = []
cantidadesDeAmigos red (u:us) = cantidadDeAmigos red u : cantidadesDeAmigos red us


-- Ejercicio 5

-- estaRobertoCarlos chequea si existe algún usuario de la red que tenga más de 10 amigos. 

estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos ([],[],[]) = False
estaRobertoCarlos red = (mayorCantidadDeAmigos red) > 10


-- Ejercicio 6

-- publicacionesDe evalúa publicacionesDeAux en las publicaciones de la red y elimina las repetidas.

publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red u = eliminarRepetidos (publicacionesDeAux (publicaciones red) u)
--acá tambien deberíamos sacar eliminarRepetidos porque la función requiere una red válida y una red válida no contiene
--publicaciones repetidas. 

-- publicacionesDeAux toma una lista de publicaciones, un usuario y devuelve una lista con las publicaciones del mismo.

publicacionesDeAux :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesDeAux [] u = []
publicacionesDeAux (p:ps) u | u == usuarioDePublicacion p = p : publicacionesDeAux ps u 
                            | otherwise = publicacionesDeAux ps u 


-- Ejercicio 7

-- publicacionesQueLeGustanA es la evaluación de publicacionesQueLeGustanAaux en la lista de publicaciones de la red.

publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA red u = eliminarRepetidos (publicacionesQueLeGustanAaux (publicaciones red) u)
--acá lo mismo con eliminarRepetidos

{-
publicacionesQueLeGustanAaux dada una lista de publicaciones y un usuario, devuelve una lista con las publicaciones que le
gustaron a ese usuario. 
-}

publicacionesQueLeGustanAaux :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesQueLeGustanAaux [] u = []
publicacionesQueLeGustanAaux (p:ps) u |pertenece u (likesDePublicacion p) =  p : publicacionesQueLeGustanAaux ps u 
                                      | otherwise = publicacionesQueLeGustanAaux ps u 


-- Ejercicio 8

{-
lesGustanLasMismasPublicaciones dada una red social y dos usuarios devuelve un booleano que indica si a esos usuarios les 
gustan las mismas publicaciones llamando a la función publicacionesQueLeGustanA y mismosElementos.
-}

lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2 = mismosElementos (publicacionesQueLeGustanA red u1) (publicacionesQueLeGustanA red u2)


-- Ejercicio 9

tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel red u | publicacionesDe red u == [] = False
                          | otherwise = tieneUnSeguidorFielAux (usuarios red) (listasDeLikesDeUsuario (publicacionesDe red u) u)


tieneUnSeguidorFielAux :: [Usuario] -> [[Usuario]] -> Bool
tieneUnSeguidorFielAux [] ls = False
tieneUnSeguidorFielAux (u:us) ls | esSeguidorFiel u ls = True
                                 | otherwise = tieneUnSeguidorFielAux us ls


esSeguidorFiel :: Usuario -> [[Usuario]] -> Bool
esSeguidorFiel u [] = True
esSeguidorFiel u (l:ls) | l == [] = False
                        | pertenece u l = esSeguidorFiel u ls
                        | otherwise = False


-- Devuelve una lista de usuarios que likearon una publicacion
listasDeLikesDeUsuario :: [Publicacion] -> Usuario -> [[Usuario]]
listasDeLikesDeUsuario [] _ = []
listasDeLikesDeUsuario (p:ps) u | pertenece u (likesDePublicacion p) = quitar u (likesDePublicacion p) : listasDeLikesDeUsuario ps u
                                | otherwise = (likesDePublicacion p) : listasDeLikesDeUsuario ps u


-- Ejercicio 10

existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red u1 u2 = u1 /= u2 && ordenados us u1 u2 && cadenaDe
Amigos (usuariosEntre us u1 u2) red
    where us = usuarios red


ordenados :: [Usuario] -> Usuario -> Usuario -> Bool
ordenados (u:us) u1 u2 | u1 == u = True
                       | u2 == u = False
                       | otherwise = ordenados us u1 u2

usuariosEntre :: [Usuario] -> Usuario -> Usuario -> [Usuario]
usuariosEntre us u1 u2 = quitarDesde u2 (quitarHasta u1 us)

quitarHasta :: Usuario -> [Usuario] -> [Usuario]
quitarHasta u1 (u:us) | u1 == u = u:us
                      | otherwise = quitarHasta u1 us

quitarDesde :: Usuario -> [Usuario] -> [Usuario]
quitarDesde u2 us | ultimo us == u2 = us
                  | otherwise = quitarDesde u2 (quitar (ultimo us) us)


-- Predicados auxiliares

-- Devuelve el entero mas grande de una lista
maximo :: [Integer] -> Integer
maximo (x:xs) | xs == [] = x
              | x >= head xs = maximo (x : tail xs)
              | x < head xs = maximo xs

-- Elimina todos los elementos que se repiten en una lista
eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) = x : eliminarRepetidos (quitarTodos x xs)

-- Elimina todas las apariciones de un elemento de la lista
quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos _ [] = []
quitarTodos x ls | quitar x ls == ls = ls
                 | otherwise = quitarTodos x (quitar x ls)

-- Saca un elemento especifico de la lista
quitar :: (Eq t) => t -> [t] -> [t]
quitar n (x:xs) | not (pertenece n (x:xs)) = (x:xs)
                | n == x = xs
                | otherwise = x : quitar n xs

-- Devuelve true si el elemento pertenece a la lista
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece n (x:xs) | n == x = True
                   | otherwise = pertenece n xs

-- Devuelve true si las 2 listas tienen los mismos elementos
mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos ls1 ls2 = listaPerteneceLista ls1 ls2 && listaPerteneceLista ls2 ls1

-- Se fija si la primer lista es subconjunto de la segunda
listaPerteneceLista :: (Eq t) => [t] -> [t] -> Bool
listaPerteneceLista [] ls = True
listaPerteneceLista (x:xs) ls = (pertenece x ls) && listaPerteneceLista xs ls

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

largo :: (Eq t) => [t] -> Integer
largo [] = 0
largo (x:xs) = largo xs + 1