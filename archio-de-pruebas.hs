-- Intento de Ejercicio 1

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])


idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

noHayRelacionesRepetidas :: [Relacion] -> Bool
noHayRelacionesRepetidas [(x)]  = [fst(x) /= sn()]

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
soloElID :: [Usuario] -> [Integer]
soloElID [(x)] = [fst(x)]
soloElID (x:xs) = fst(x) : soloElID xs


noHayIdRepetidos :: [Usuario] -> Bool
noHayIdRepetidos us = todosDistintos (soloElID (us))

usuariosValidos :: [Usuario] -> Bool
usuariosValidos [] = True
usuariosValidos (x:xs) | usuarioValido (x) == (noHayIdRepetidos (x:xs)) = usuariosValidos xs  
                       | otherwise = False



































