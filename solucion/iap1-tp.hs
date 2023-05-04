-- Nombre de Grupo: HaskellyGretel
-- Integrante 1: Nombre Apellido, email, LU
-- Integrante 2: Nombre Apellido, email, LU
-- Integrante 3: Nombre Apellido, email, LU
-- Integrante 4: Nombre Apellido, email, LU

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

-- Funciones basicas

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

-- nombresDeUsuarios recibe una redSocial valida y devuelve una lista con los nombres de los usuarios de la red social 
-- donde no hay repetidos, es decir, si dos usuarios se llaman carlitos en la lista aparecerá una sola vez "carlitos".

nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios ([], _, _) = []
nombresDeUsuarios (u:us, rs, ps) | pertenece (nombreDeUsuario u) restoUsuarios = restoUsuarios
                                 | otherwise = (nombreDeUsuario u): restoUsuarios
                                 where restoUsuarios = nombresDeUsuarios (us, rs, ps)

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece e (x:xs) | e == x = True 
                   | otherwise = pertenece e xs

-- La funcion amigosDe recibe una redSocial y un usario (ambos válidos, donde además el usuario pertenece a esa red), y devuelve una lista 
-- con los usuarios que se relacionan con el.

amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe (_, [], _) _ = []
amigosDe (us, (r1,r2):rs, ps) u | r1 == u = r2: restoDeAmigos
                                | r2 == u = r1: restoDeAmigos 
                                | otherwise = restoDeAmigos
                                where restoDeAmigos = amigosDe (us, rs, ps) u

-- cantidadDeAmigos recibe una redSocial y un usuario (ambos válidos, donde además el usuario pertenece a esa red) 
-- y devuelve la cantidad de amigos que ese usuario posee en esa red social.

cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red u = longitud (amigosDe red u)

longitud :: [t] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- usuarioConMasAmigos recibe una redSocial (válida y con al menos un usuario) y devuelve al usuario con mas amigos de la misma.
-- Obs: Si hay 2 o mas usuarios con la misma cantidad de amigos la funcion va a devolver al primero de ellos que aparezca.

usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos ([x], _, _) = x
usuarioConMasAmigos (u1:u2:us, rs, ps) | (cantidadDeAmigos red u1) >= (cantidadDeAmigos red u2) = usuarioConMasAmigos (u1:us, rs, ps)
                                       | otherwise = usuarioConMasAmigos (u2:us, rs, ps)
                                       where red = (u1:u2:us, rs, ps)

-- estaRobertoCarlos recibe una redSocial y devuelve True si hay un usuario con mas de 1 millon de amigos. Ademas, si se le pasa una red
-- sin usuarios devuelve False (Es necesario aclarar este caso pues usuarioConMasAmigos esta def. solo para redes con al menos 1 amigo)

estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos ([], _, _) = False
estaRobertoCarlos red = cantidadDeAmigos red (usuarioConMasAmigos red) > 1000000

-- publicacionesDe recibe una redSocial y un usuario que pertenezca a ella, y devuelve una lista con sus publicaciones (En caso de haberlas,
-- sino retorna la lista vacia.)

publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe (_, _, []) _ = []
publicacionesDe (us, rs, p:ps) u | (usuarioDePublicacion p) == u = p: (publicacionesDe (us, rs, ps) u)
                                 | otherwise = publicacionesDe (us, rs, ps) u 

-- publicacionesQueLeGustanA recibe una redSocial y un usuario que pertenece a esta y devuelve una lista con las publicaciones que le gustaron.
-- En caso de que no le haya gustado ninguna devuelve la lista vacia.

publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA (_, _, []) _ = []
publicacionesQueLeGustanA (us, rs, p:ps) u | pertenece u (likesDePublicacion p) = p: (publicacionesQueLeGustanA (us, rs, ps) u)
                                           | otherwise = publicacionesQueLeGustanA (us, rs, ps) u 

-- lesGustanLasMismaPublicaciones recibe una redSocial y dos usuarios, devolviendo True si a ambos les gustan las mismas publicaciones.

lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2 = sonIguales (publicacionesQueLeGustanA red u1) (publicacionesQueLeGustanA red u2)

-- sonIguales recibe dos conjuntos y devuelve True si son iguales, es decir, cumplen la doble inclusion.
-- Uso esta funcion puesto que no me importa el orden de la lista de publicaciones que devuelve publicacionesQueLeGustanA.

sonIguales :: (Eq t) => [t] -> [t] -> Bool
sonIguales xs ys = contenido xs ys && contenido ys xs

-- contenido recibe dos conjuntos xs e ys y devuelve True si xs esta contenido en ys

contenido :: (Eq t) => [t] -> [t] -> Bool
contenido [] _ = True
contenido (x:xs) ys | pertenece x ys = contenido xs ys
                    | otherwise = False
                     
-- tieneUnSeguidorFiel recibe una redSocial y un usuario (u) de la misma y devuelve True si existe un usuario que le gusten todas las publicaciones de u
-- en la red social dada. Ademas, u tiene que ser distinto de u1, es decir, no se puede ser su propio seguidor fiel.

tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel ([], rs, ps) _ = False
tieneUnSeguidorFiel (u:us, rs, ps) u1 | contenido publicacionesU1 (publicacionesQueLeGustanA red u) && (longitud publicacionesU1) > 0 && u /= u1 = True 
                                      | otherwise = tieneUnSeguidorFiel restoRed u1
                                      where publicacionesU1 = publicacionesDe red u1
                                            red = (u:us, rs, ps)
                                            restoRed = (us, rs, ps)

-- existeSecuenciaDeAmigos recibe una redSocial y dos usuarios (u1 y u2) y devuelve True en caso de que exista un 
-- subconjunto de usuarios tales que su primer elemento sea u1, su ultimo elemento sea u2 y cada usuario se relacione
-- con el siguiente.

-- Observacion : combinacionesPosibles es una lista de listas de usuarios que contiene todas las posibles maneras de agrupar usuarios.
-- donde se distingue a [usuario1, usuario2, usuario3] de [usuario2, usuario1, usuario3]. 
-- ¿Como funciona? realizo Partes de us (donde us es la lista de usuarios de la red) y de esta manera consigo todos los posibles subconjuntos
-- de us, y luego con permutarElementos obtengo todas las maneras de ordenar esos subconjuntos.

existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos (u:us, rs, ps) u1 u2 = (hayCadenaDeAmigos combinacionesPosibles (u:us, rs, ps) u1 u2)
                                             where combinacionesPosibles = permutarElementos (partes (u:us))

-- Funciones auxiliares para existeSecuenciaDeAmigos --

-- relacionadosDirecto recibe dos usuarios de una red y devuelve True si ambos son amigos.

relacionadosDirecto :: Usuario -> Usuario -> RedSocial -> Bool
relacionadosDirecto u1 u2 red = pertenece u1 (amigosDe red u2) 

-- cadenaDeAmigos recibe una lista de usuarios y si cada usuario de la lista se relaciona con el siguiente devuelve True.

cadenaDeAmigos :: [Usuario] -> RedSocial -> Bool
cadenaDeAmigos [_] _ = False
cadenaDeAmigos [u1,u2] red = relacionadosDirecto u1 u2 red
cadenaDeAmigos (u:us) red | relacionadosDirecto u (head us) red = cadenaDeAmigos us red
                          | otherwise = False

-- hayCadenadeAmigos recibe una lista de de listas de usuarios de una redSocial y la redSocial y devuelve True si alguna lista de
-- usuarios forma una cadena de amigos y ademas comienza con u1 y temina con u2.

hayCadenaDeAmigos :: [[Usuario]] -> RedSocial -> Usuario -> Usuario -> Bool
hayCadenaDeAmigos [[]] _ _ _ = False
hayCadenaDeAmigos (u:us) red u1 u2 | cadenaDeAmigos u red && empiezaConU1 && terminaConU2 = True
                                   | otherwise = hayCadenaDeAmigos us red u1 u2
                                    where empiezaConU1 = (head u) == u1
                                          terminaConU2 = (ultimo u) == u2
                                          ultimo (x:xs) | (longitud xs == 0) = x
                                                        | otherwise = ultimo xs

-- Defino la funcion partes para luego poder agrupar a los usuarios que reciba la funcion
-- de todas las maneras posibles. Partes recibe una lista (xs) y devuelve una lista de listas con todos los subconjuntos de la lista xs

partes :: [t] -> [[t]]
partes [] = [[]]
partes (x:xs) = agregarATodos x (partes xs) ++ partes xs

agregarATodos :: t -> [[t]] -> [[t]]
agregarATodos n [] = []
agregarATodos n (x:xs) = (n: x): agregarATodos n xs

-- permutaciones recibe una lista (xs) y devuelve una lista con todas las permutaciones de xs. La defino con el fin de 
-- obtener todas las permutaciones de elementos de partes de A.

permutaciones :: (Eq t) => [t] -> [[t]]
permutaciones [] = [[]]
permutaciones (x:xs) = insertarEnCadaPosDeTodasLasListas (permutaciones xs) x

-- Defino las funciones necesarias para hacer permutaciones --

union :: (Eq t) => [t] -> [t] -> [t]
union [] ys = ys
union (x:xs) ys = union xs (agregar x ys)

agregar :: (Eq t) => t -> [t] -> [t]
agregar e l | pertenece e l = l
            | otherwise = e:l

insertarEn :: [t] -> t -> Int -> [t]
insertarEn l e 0 = e:l
insertarEn (x:xs) e n = x: (insertarEn xs e (n-1))

insertarEnCadaPos :: (Eq t) => [t] -> t -> Int -> [[t]]
insertarEnCadaPos l e 0 = agregar (insertarEn l e 0) []
insertarEnCadaPos l e n = agregar (insertarEn l e n) (insertarEnCadaPos l e (n-1))

insertarEnCadaPosDeTodasLasListas :: (Eq t) => [[t]] -> t -> [[t]]
insertarEnCadaPosDeTodasLasListas [] e = []
insertarEnCadaPosDeTodasLasListas (x:xs) e = union (insertarEnCadaPos x e (longitud x)) (insertarEnCadaPosDeTodasLasListas xs e)

-- permutarElementos recibe una lista de listas de t y devuelve una lista de listas de t con todas 
-- las permutaciones posibles de cada lista de t. 

permutarElementos :: (Eq t) => [[t]] -> [[t]]
permutarElementos [[]] = [[]]
permutarElementos (x:xs) = (permutaciones x) ++ permutarElementos xs

-- quitar recibe un elemento y una lista y saca la primera aparicion de este en la lista. (Como estoy trabajando con "conjuntos", cada elemento 
-- va a aparecer una vez en la lista.)

quitar :: (Eq t) => t -> [t] -> [t]
quitar _ [] = []
quitar y (x:xs) | x == y = xs
                | otherwise = x: quitar y xs



usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")

relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1)
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)

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