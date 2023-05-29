
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios ([],[],[]) = []
nombresDeUsuarios red = agregarSinRepetidos (usuarios red)

agregarSinRepetidos :: [Usuario] -> [String]
agregarSinRepetidos [] = []
agregarSinRepetidos (u:us) = snd u : agregarSinRepetidos(eliminarRepetidos us)

--necesitaria que eliminarRepetidos funcione solo con los nombres, osea como ej
--eliminarR de una lista de usuarios que tiene a u1 = (1,"Juan") y u2 = (2,"Juan")
--deberia eliminar a uno de los nombres a pesar de que u1 /= u2

--(0,Ana) (1,Brisa) (2, Carla) (3,Diego) (5,Diego) 
--0 : agregarSinRe(eliminarRe((1,Brisa) (2, Carla) (3,Diego) (5,Diego)))

--quitar :: Usuario -> [Usuario] -> [Usuario]
--quitar u (x:xs) | not (pertenece (snd u) (x:xs)) = (x:xs)
--                | u == snd x = xs
--                | otherwise = x : quitar u xs





listaAmistades :: [Relacion] -> Usuario -> [Usuario]
listaAmistades [] u = []
listaAmistades (r:rs) u | fst r == u = snd r : listaAmistades rs u
                        | snd r == u = fst r : listaAmistades rs u
                        | otherwise = listaAmistades rs u

--con pattern matching:

listaAmistades :: [Relacion] -> Usuario -> [Usuario]
listaAmistades [] u = []
listaAmistades ((u,v):rs) u = snd r : listaAmistades rs u
listaAmistades ((v,u):rs) u = fst r : listaAmistades rs u
listaAmistades (r:rs) u = listaAmistades rs u 




estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos ([],[],[]) = False
estaRobertoCarlos red = cantidadDeAmigos red (usuarioConMasAmigos red) > 10