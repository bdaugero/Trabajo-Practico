module Solucion where

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

-- Toma una red social y llama a proyectarNombres con los usuarios de la red y devuelve los nombres de los usuarios de la red.
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios ([],[],[]) = []
nombresDeUsuarios red = proyectarNombres (usuarios red)

-- Toma una lista de usuarios, llama a la funcion soloNombresUsuarios y elimina los repetidos.
proyectarNombres :: [Usuario] -> [String]
proyectarNombres us = eliminarRepetidos (soloNombresUsuarios us)

-- Toma una lista de usuarios y devuelve una lista con los nombres de esos usuarios (admite repetidos).
soloNombresUsuarios :: [Usuario] -> [String]
soloNombresUsuarios [u] = [snd u]
soloNombresUsuarios (u:us) = snd u : soloNombresUsuarios us


-- Ejercicio 2

-- Llama a la función auxiliar listaAmistades, evaluándola en la lista de relaciones de la red social.
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe red u = listaAmistades (relaciones red) u


-- Dada una lista de relaciones y un usuario, devuelve una lista con los amigos del usuario.
listaAmistades :: [Relacion] -> Usuario -> [Usuario]
listaAmistades [] u = []
listaAmistades (r:rs) u | fst r == u = snd r : listaAmistades rs u
                        | snd r == u = fst r : listaAmistades rs u
                        | otherwise = listaAmistades rs u


-- Ejercicio 3

-- Dado un usuario y una red, devuelve la cantidad de elementos de amigosDe el usuario.
cantidadDeAmigos :: RedSocial -> Usuario -> Integer
cantidadDeAmigos red u = largo (amigosDe red u)


-- Ejercicio 4

-- Es la evaluación de usuarioConMasAmigosAux en la lista de usuarios de la red.
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos red = usuarioConMasAmigosAux red (usuarios red)


-- Va recorriendo la lista cantidadesDeAmigos hasta que la cantidad de amigos de un usuario coincida con mayorCantidadDeAmigos de la red, y devuelve ese usuario.
usuarioConMasAmigosAux :: RedSocial -> [Usuario] -> Usuario
usuarioConMasAmigosAux red [u] = u
usuarioConMasAmigosAux red (u:us) | mayorCantidadDeAmigos red == cantidadDeAmigos red u = u
                                  | otherwise = usuarioConMasAmigosAux red us


-- Dada una red social devuelve la cantidad máxima de usuarios que existe en la red.
mayorCantidadDeAmigos :: RedSocial -> Integer
mayorCantidadDeAmigos red = maximo (cantidadesDeAmigos red (usuarios red))


-- Dada una red social y una lista de usuarios de la red, devuelve una lista con la cantidad de amigos que tiene cada usuario.
cantidadesDeAmigos :: RedSocial -> [Usuario] -> [Integer]
cantidadesDeAmigos red [] = []
cantidadesDeAmigos red (u:us) = cantidadDeAmigos red u : cantidadesDeAmigos red us


-- Ejercicio 5

-- Chequea si existe algún usuario de la red que tenga más de 10 amigos. 
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos ([],[],[]) = False
estaRobertoCarlos red = (mayorCantidadDeAmigos red) > 10


-- Ejercicio 6

-- Evalúa publicacionesDeAux en las publicaciones de la red y elimina las repetidas.
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red u = publicacionesDeAux (publicaciones red) u

-- Toma una lista de publicaciones, un usuario y devuelve una lista con las publicaciones del mismo.
publicacionesDeAux :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesDeAux [] u = []
publicacionesDeAux (p:ps) u | u == usuarioDePublicacion p = p : publicacionesDeAux ps u 
                            | otherwise = publicacionesDeAux ps u 


-- Ejercicio 7

-- Es la evaluación de publicacionesQueLeGustanAaux en la lista de publicaciones de la red.
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA red u = publicacionesQueLeGustanAaux (publicaciones red) u



-- Dada una lista de publicaciones y un usuario, devuelve una lista con las publicaciones que le gustaron a ese usuario. 
publicacionesQueLeGustanAaux :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesQueLeGustanAaux [] u = []
publicacionesQueLeGustanAaux (p:ps) u |pertenece u (likesDePublicacion p) =  p : publicacionesQueLeGustanAaux ps u 
                                      | otherwise = publicacionesQueLeGustanAaux ps u 


-- Ejercicio 8


-- Dada una red social y dos usuarios verifica que tengan mismosElementos las publicacionesQueLeGustanA los dos usuarios.  
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2 = mismosElementos (publicacionesQueLeGustanA red u1) (publicacionesQueLeGustanA red u2)


-- Ejercicio 9

-- Toma una red, un usuario y llama a tieneUnSeguidorFielAux con los usuarios de la red y las listasDeLikesDeUsuario de las publicaciones del usuario dado. 
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel red u | publicacionesDe red u == [] = False
                          | otherwise = tieneUnSeguidorFielAux (usuarios red) (listasDeLikesDeUsuario (publicacionesDe red u) u)

-- Toma los usuarios de la red y la lista de likes de las publicaciones de un usuario y verifica si un usuario de la red esSeguidorFiel del usuario.
tieneUnSeguidorFielAux :: [Usuario] -> [[Usuario]] -> Bool
tieneUnSeguidorFielAux [] ls = False
tieneUnSeguidorFielAux (u:us) ls | esSeguidorFiel u ls = True
                                 | otherwise = tieneUnSeguidorFielAux us ls

-- Veirifica que un usuario pertenezca a todos los likes de las publicaciones de un usuario. 
esSeguidorFiel :: Usuario -> [[Usuario]] -> Bool
esSeguidorFiel u [] = True
esSeguidorFiel u (l:ls) | l == [] = False
                        | pertenece u l = esSeguidorFiel u ls
                        | otherwise = False


-- Devuelve una lista de likes de las publicaciones de un usuario (likes: [Usuario]).
listasDeLikesDeUsuario :: [Publicacion] -> Usuario -> [[Usuario]]
listasDeLikesDeUsuario [] _ = []
listasDeLikesDeUsuario (p:ps) u | pertenece u (likesDePublicacion p) = quitar u (likesDePublicacion p) : listasDeLikesDeUsuario ps u
                                | otherwise = (likesDePublicacion p) : listasDeLikesDeUsuario ps u


-- Ejercicio 10

-- Ingresados una red y dos usuarios verifica que ambos usuarios ingresados sean distintos, esten ordenados en los usuarios de la red y haya cadenaDeAmigos entre ellos en la red.
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red u1 u2 = u1 /= u2 && ordenados us u1 u2 && cadenaDeAmigos (usuariosEntre us u1 u2) red
    where us = usuarios red

-- Dados una lista de usuarios y dos usuarios verifica que esten en la lista en el orden ingresado (no importa si hay usuarios entre ellos). 
ordenados :: [Usuario] -> Usuario -> Usuario -> Bool
ordenados (u:us) u1 u2 | u1 == u = True
                       | u2 == u = False
                       | otherwise = ordenados us u1 u2

-- Dados una lista de usuarios y dos usuarios de la lista devuelve una lista que empieza con el primero ingresado, termina con el segundo ingresado y conserva los usuarios que habia entre ellos en la lista original.
usuariosEntre :: [Usuario] -> Usuario -> Usuario -> [Usuario]
usuariosEntre us u1 u2 = quitarDesde u2 (quitarHasta u1 us)


-- Dado un usuario y una lista de usuarios devuelve una lista sin los usuarios previos al usuario ingresado.
quitarHasta :: Usuario -> [Usuario] -> [Usuario]
quitarHasta u1 (u:us) | u1 == u = u:us
                      | otherwise = quitarHasta u1 us


-- Dado un usuario y una lista de usuarios devuelve una lista sin los usuarios posteriores al usuario ingresado. 
quitarDesde :: Usuario -> [Usuario] -> [Usuario]
quitarDesde u2 us | ultimo us == u2 = us
                  | otherwise = quitarDesde u2 (quitar (ultimo us) us)


-- Predicados auxiliares

-- Devuelve el entero mayor de una lista
maximo :: [Integer] -> Integer
maximo (x:xs) | xs == [] = x
              | x >= head xs = maximo (x : tail xs)
              | x < head xs = maximo xs

-- Elimina todos los elementos que se repiten en una lista conservando una unica aparicion
eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) = x : eliminarRepetidos (quitarTodos x xs)

-- Elimina todas las apariciones de un elemento de la lista
quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos _ [] = []
quitarTodos x ls | quitar x ls == ls = ls
                 | otherwise = quitarTodos x (quitar x ls)

-- Saca la primera aparicion de un elemento especifico de la lista.
quitar :: (Eq t) => t -> [t] -> [t]
quitar n (x:xs) | not (pertenece n (x:xs)) = (x:xs)
                | n == x = xs
                | otherwise = x : quitar n xs

-- Devuelve true si el elemento pertenece a la lista.
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece n (x:xs) | n == x = True
                   | otherwise = pertenece n xs

-- Devuelve true si las 2 listas tienen los mismos elementos y misima longitud.
mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos ls1 ls2 = (largo ls1 == largo ls2) && listaPerteneceLista ls1 ls2 && listaPerteneceLista ls2 ls1

-- Verifica si los elementos de la primer lista pertenecen a la segunda.
listaPerteneceLista :: (Eq t) => [t] -> [t] -> Bool
listaPerteneceLista [] ls = True
listaPerteneceLista (x:xs) ls = (pertenece x ls) && listaPerteneceLista xs ls

-- Verifica que cada usuario de la lista esta relacionadosDirecto con el siguiente de la misma. 
cadenaDeAmigos :: [Usuario] -> RedSocial -> Bool
cadenaDeAmigos (a:b:[]) red = relacionadosDirecto a b red  
cadenaDeAmigos (u:us) red | relacionadosDirecto u (head us) red = cadenaDeAmigos us red
                          | otherwise = False

-- Verifica que haya una relacion entre los dos usuarios en la red.
relacionadosDirecto :: Usuario -> Usuario -> RedSocial -> Bool
relacionadosDirecto u1 u2 red = pertenece (u1, u2) (relaciones red) || pertenece (u2, u1) (relaciones red)

-- Devuelve el ultimo elemento de una lista.
ultimo :: [t] -> t
ultimo [a] = a
ultimo (x:xs) = ultimo xs

-- Devuelve la cantidad de elementos de una lista.
largo :: (Eq t) => [t] -> Integer
largo [] = 0
largo (x:xs) = largo xs + 1