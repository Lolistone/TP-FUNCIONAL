-- Nombre de Grupo: HaskellyGretel
-- Integrante 1: Lisandro Cordoba Lazzar, lisandrocordoba11@gmail.com, 327/23
-- Integrante 2: Lorenzo Martinelli, martinelli.lorenzo12@gmail.com , 364/23
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
nombresDeUsuarios (u:us, rs, ps) = union [nombreDeUsuario u] restoUsuarios
                                 where restoUsuarios = nombresDeUsuarios (us, rs, ps)

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

-- existeSecuenciaDeAmigos recibe una redSocial y dos usuarios (u1 y u2) y devuelve True en caso de que exista una 
-- subsecuencia de usuarios tales que su primer elemento sea u1, su ultimo elemento sea u2 y cada usuario se relacione
-- con el siguiente.

existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red u1 u2 = existeSecuenciaDeAmigosAUX red [u1] u2

-- existeSecuenciaDeAmigosAUX es precisamente una funcion auxiliar para existeSecuenciaDeAmigos que recibe una RedSocial, una lista 
-- de usuarios us y un usuario u2, y devuelve True si algun usuario de us forma una cadena de amigos con u2.
 
existeSecuenciaDeAmigosAUX :: RedSocial -> [Usuario] -> Usuario -> Bool
existeSecuenciaDeAmigosAUX ([], _, _) us u2 = False
existeSecuenciaDeAmigosAUX (_, [], _) us u2 = False
existeSecuenciaDeAmigosAUX red us u2 | pertenece u2 (amigosDeVarios us red) = True
                                     | otherwise = existeSecuenciaDeAmigosAUX (quitarTodosDeLaRed us red) (amigosDeVarios us red) u2

-- amigosDeVarios recibe una lista de usuarios us y una redSocial y devuelve una lista con todos los amigos de los ususarios pertenecientes a us. 

amigosDeVarios :: [Usuario] ->  RedSocial -> [Usuario]
amigosDeVarios [] red = []
amigosDeVarios [u] red = amigosDe red u
amigosDeVarios (u:us) red = union (amigosDe red u) (amigosDeVarios us red)

-- union recibe dos listas y devuelve su union, es decir, devuelve una lista con los elementos que estaban en cada lista
-- sin repetidos.

union :: (Eq t) => [t] -> [t] -> [t]
union [] ys = ys
union (x:xs) ys = union xs (agregar x ys)
                  where agregar e l | pertenece e l = l
                                    | otherwise = e:l

-- pertenece recibe un elemento de tipo t y una lista de tipo t y devuelve True si este pertenece a la lista.

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece e (x:xs) | e == x = True 
                   | otherwise = pertenece e xs

-- quitar recibe un elemento y una lista y saca la primera aparicion de este en la lista.

quitar :: (Eq t) => t -> [t] -> [t]
quitar _ [] = []
quitar y (x:xs) | x == y = xs
                | otherwise = x: quitar y xs

-- eliminarRelacionesDe recibe un usuario y una lista de relaciones y devuelve la lista de relacion sin 
-- las relaciones donde aparece el usuario.

eliminarRelacionesDe :: Usuario -> [Relacion] -> [Relacion]
eliminarRelacionesDe _ [] = []
eliminarRelacionesDe u ((u1,u2):rs) | u == u1 || u == u2 = eliminarRelacionesDe u rs
                                    | otherwise = (u1,u2): eliminarRelacionesDe u rs

-- eliminarPublicacionesDe recibe un usuario, una lista de Publicaciones y una RedSocial, y devuelve la lista de publicaciones sin 
-- las publicaciones de ese usuario en la red dada.

eliminarPublicacionesDe :: Usuario -> [Publicacion] -> RedSocial -> [Publicacion]
eliminarPublicacionesDe _ [] _ = []
eliminarPublicacionesDe u (p:ps) red | pertenece p (publicacionesDe red u) = eliminarPublicacionesDe u ps red
                                     | otherwise = p: eliminarPublicacionesDe u ps red

-- quitarDeLaRed recibe un usuario y una red social y devuelve la red social sin ese usario (lo elimina tanto a el como a sus publicaciones
-- y a sus amigos). Aclaracion: no consideramos necesario eliminar sus likes de las publicaciones para el fin que le ibamos a dar a esta
-- funcion.

quitarDeLaRed :: Usuario -> RedSocial -> RedSocial
quitarDeLaRed u (us, rs, ps) = (quitar u us, eliminarRelacionesDe u rs, eliminarPublicacionesDe u ps (us,rs,ps))

-- quitarTodosDeLaRed recibe una lista de usuarios y una red social y devuelve la red social sin esos usarios.

quitarTodosDeLaRed :: [Usuario] -> RedSocial -> RedSocial
quitarTodosDeLaRed [] red = red
quitarTodosDeLaRed (u:us) red = quitarTodosDeLaRed us (quitarDeLaRed u red)
