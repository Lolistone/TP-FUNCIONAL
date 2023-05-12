import Test.HUnit
import Solucion

main = runTestTT tests
run1 = runTestTT testNombresDeUsuarios
run2 = runTestTT testAmigosDe
run3 = runTestTT testCantidadDeAmigos
run4 = runTestTT testUsuarioConMasAmigos 
run5 = runTestTT testEstaRobertoCarlos
run6 = runTestTT testPublicacionesDe
run7 = runTestTT testPublicacionesQueLeGustanA
run8 = runTestTT testLesGustanLasMismasPublicaciones
run9 = runTestTT testTieneUnSeguidorFiel
run10 = runTestTT testExisteSecuenciaDeAmigos
runAll = runTestTT testAll -- corre todos los casos inclusive los test provistos por la catedra

tests = test [
    " nombresDeUsuarios 1" ~: (nombresDeUsuarios redA) ~?= ["Juan","Natalia","Pedro","Mariela"],

    " amigosDe 1" ~: (amigosDe redA usuario1) ~?= [usuario2, usuario4],

    " cantidadDeAmigos 1" ~: (cantidadDeAmigos redA usuario1) ~?= 2,

    " usuarioConMasAmigos 1" ~: expectAny (usuarioConMasAmigos redA) [usuario2, usuario4],

    " estaRobertoCarlos 1" ~: (estaRobertoCarlos redA) ~?= False,

    " publicacionesDe 1" ~: (publicacionesDe redA usuario2) ~?= [publicacion2_1, publicacion2_2],

    " publicacionesQueLeGustanA 1" ~: (publicacionesQueLeGustanA redA usuario1) ~?= [publicacion2_2, publicacion4_1],

    " lesGustanLasMismasPublicaciones 2" ~: (lesGustanLasMismasPublicaciones redB usuario1 usuario3) ~?= True,

    " tieneUnSeguidorFiel 1" ~: (tieneUnSeguidorFiel redA usuario1) ~?= True,

    " existeSecuenciaDeAmigos 1" ~: (existeSecuenciaDeAmigos redA usuario1 usuario3) ~?= True
 ]

-- Nuestros test suite para cada ejercicio.

testNombresDeUsuarios = test [
    "Caso 1: un solo usuario" ~: (nombresDeUsuarios redUnica) ~?= ["Juan"],
    "Caso 2: usuarios disintos" ~: (nombresDeUsuarios redA) ~?= ["Juan","Natalia","Pedro","Mariela"],
    "Caso 3: usuarios repetidos" ~: (nombresDeUsuarios redB) ~?= ["Juan", "Pedro", "Natalia"], -- (1)
    "Caso 4: sin usuarios" ~: (nombresDeUsuarios redVacia) ~?= []
 ]

-- (1) Orden elegido arbitrariamente, pues con expectAny deberiamos haber tipeado 6 posibilidades.

testAmigosDe = test [
    "Caso 1: un solo usuario" ~: (amigosDe redUnica usuario1) ~?= [],
    "Caso 2: dos usuarios, son amigos" ~: (amigosDe redC usuario1) ~?= [usuario2],
    "Caso 3: dos usuarios, no son amigos" ~: (amigosDe redD usuario1) ~?= [],
    "Caso 4: varios usuarios, el usuario no tiene amigos" ~: (amigosDe redB usuario5) ~?= [],
    "Caso 5: varios usuarios, el usuario tiene mas de un amigo" ~: (amigosDe redA usuario2) ~?= [usuario1, usuario3, usuario4],
    "Caso 6: varios usuarios, el usuario tiene un amigo" ~: (amigosDe redB usuario1) ~?= [usuario2]
 ]

testCantidadDeAmigos = test [
    "Caso 1: un solo usuario" ~: (cantidadDeAmigos redUnica usuario1) ~?= 0,
    "Caso 2: varios usuarios, el usuario no tiene amigos" ~: (cantidadDeAmigos redB usuario5) ~?= 0,
    "Caso 3: el usuario tiene 1 amigo" ~: (cantidadDeAmigos redB usuario1) ~?= 1,
    "Caso 4: el usuario tiene varios amigos" ~: (cantidadDeAmigos redA usuario2) ~?= 3
 ]

testUsuarioConMasAmigos = test [
    "Caso 1: un solo usuario" ~: (usuarioConMasAmigos redUnica) ~?= usuario1,
    "Caso 2: varios usuarios, uno con mas que el resto" ~: (usuarioConMasAmigos redB) ~?= usuario2,
    "Caso 3: varios usuarios, misma cantidad amigos" ~: expectAny (usuarioConMasAmigos redC) [usuario1, usuario2],
    "Caso 4: varios usuarios, ninguno tiene amigos" ~: expectAny (usuarioConMasAmigos redE) [usuario1, usuario2, usuario3]
 ]

testEstaRobertoCarlos = test [
    "Caso 1: sin usuarios" ~: (estaRobertoCarlos redVacia) ~?= False,
    "Caso 2: un solo usuario" ~: (estaRobertoCarlos redUnica) ~?= False,
    "Caso 3: menos de 10 usuarios" ~: (estaRobertoCarlos redA) ~?= False,
    "Caso 4: mas de 10 usuarios, alguno tiene mas de 10 amigos" ~: (estaRobertoCarlos redRoberto) ~?= True,
    "Caso 5: mas de 10 usuarios, ninguno tiene mas de 10 amigos" ~: (estaRobertoCarlos redCasiRoberto) ~?= False
 ]

testPublicacionesDe = test [
    "Caso 1: ninguna publicacion en la red" ~: (publicacionesDe redC usuario2) ~?= [],
    "Caso 2: ninguna publicacion hecha por el usuario" ~: (publicacionesDe redB usuario2) ~?= [],
    "Caso 3: una unica publicacion hecha por el usuario" ~: (publicacionesDe redRoberto usuario10) ~?= [publicacion10_1],
    "Caso 4: mas de una publicacion hecha por el usuario" ~: (publicacionesDe redB usuario1) ~?= [publicacion1_3, publicacion1_4, publicacion1_5]
 ]

testPublicacionesQueLeGustanA = test [
    "Caso 1: no hay publicaciones" ~: (publicacionesQueLeGustanA redC usuario1) ~?= [],    
    "Caso 2: no le gusta ninguna publicacion" ~: (publicacionesQueLeGustanA redB usuario3) ~?= [],
    "Caso 3: le gusta una" ~: (publicacionesQueLeGustanA redRoberto usuario6) ~?= [publicacion10_1],
    "Caso 4: le gustan varias" ~: (publicacionesQueLeGustanA redB usuario5) ~?= [publicacion1_3, publicacion1_5, publicacion3_3],
    "Caso 5: like a si mismo" ~: (publicacionesQueLeGustanA redE usuario3) ~?= [publicacion3_4]
 ]  

-- El enunciado menciona que un usuario no puede darse like a si mismo, pues solo puede dar like a sus amigos y no puede ser su propio amigo.
-- Sin embargo, en la especificacion no aclara que no pueda dar like a su propia publicacion. Por lo tanto, nuestra implementacion cubre tal caso.

testLesGustanLasMismasPublicaciones = test [
    "Caso 1: no hay publicaciones" ~: (lesGustanLasMismasPublicaciones redC usuario1 usuario2) ~?= True,
    "Caso 2: no les gustan las mismas" ~: (lesGustanLasMismasPublicaciones redB usuario2 usuario5) ~?= False,
    "Caso 3: les gustan las mismas (Al menos dieron un like)" ~: (lesGustanLasMismasPublicaciones redRoberto usuario7 usuario8) ~?= True,
    "Caso 4: a ninguno le gusta niguna" ~: (lesGustanLasMismasPublicaciones redRoberto usuario1 usuario2) ~?= True
 ]

testTieneUnSeguidorFiel = test [
    "Caso 1: un usuario en la red" ~: (tieneUnSeguidorFiel redUnica usuario1) ~?= False,
    "Caso 2: el usuario no tiene publicaciones" ~: (tieneUnSeguidorFiel redRoberto usuario6) ~?= False,
    "Caso 3: una sola publicacion, no hay likes" ~: (tieneUnSeguidorFiel redRoberto usuario5) ~?= False,
    "Caso 4: una sola publicacion, solo like propio" ~: (tieneUnSeguidorFiel redF usuario1) ~?= False,
    "Caso 5: una sola publicacion, hay mas de un fiel" ~: (tieneUnSeguidorFiel redRoberto usuario10) ~?= True,
    "Caso 6: una sola publicacion, hay un solo fiel" ~: (tieneUnSeguidorFiel redF usuario2) ~?= True,
    "Caso 7: mas de una publicacion, no hay likes" ~: (tieneUnSeguidorFiel redF usuario3) ~?= False,
    "Caso 8: mas de una publicacion, hay likes pero no hay fiel" ~: (tieneUnSeguidorFiel redA usuario4) ~?= False,
    "Caso 9: mas de una publicacion, solo likes propios" ~: (tieneUnSeguidorFiel redF usuario5) ~?= False,
    "Caso 10: mas de una publicacion, hay un solo fiel" ~: (tieneUnSeguidorFiel redA usuario2) ~?= True,
    "Caso 11: mas de una publicacion, hay mas de un fiel" ~: (tieneUnSeguidorFiel redF usuario4) ~?= True
 ]

testExisteSecuenciaDeAmigos = test [
    "Caso 1: dos usuarios sin relacion" ~: (existeSecuenciaDeAmigos redD usuario1 usuario2) ~?= False,
    "Caso 2: dos usuarios relacionados" ~: (existeSecuenciaDeAmigos redC usuario1 usuario2) ~?= True,
    "Caso 3: mas de dos usuarios, no hay ninguna relacion" ~: (existeSecuenciaDeAmigos redE usuario1 usuario3) ~?= False,
    "Caso 4: mas de dos usuarios, hay relaciones pero no hay cadena de amigos" ~: (existeSecuenciaDeAmigos redB usuario1 usuario5) ~?= False,
    "Caso 5: mas de dos usuarios, existe cadena de amigos, u1 relacionado con u2" ~: (existeSecuenciaDeAmigos redRoberto usuario1 usuario11) ~?= True,
    "Caso 6: mas de dos usuarios, existe cadena de amigos pero u1 no se relaciona con u2" ~: (existeSecuenciaDeAmigos redRoberto usuario1 usuario7) ~?= True
 ]

testAll = test [testNombresDeUsuarios, testAmigosDe, testCantidadDeAmigos, testUsuarioConMasAmigos, testEstaRobertoCarlos, testPublicacionesDe,
                testPublicacionesQueLeGustanA, testLesGustanLasMismasPublicaciones, testTieneUnSeguidorFiel, testExisteSecuenciaDeAmigos, tests]

expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)

-- Ejemplos

usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")
usuario6 = (6, "Lorenzo")
usuario7 = (7, "Lisandro")
usuario8 = (8, "Tiago")
usuario9 = (9, "Felipe")
usuario10 = (10, "Messi")
usuario11 = (11, "Roberto Carlos")
usuario12 = (12, "Carlitos")

relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)
relacion6_10 = (usuario10, usuario6) 
relacion7_10 = (usuario10, usuario7) 
relacion8_10 = (usuario10, usuario8) 
relacion11_1 = (usuario11, usuario1) 
relacion11_2 = (usuario11, usuario2)
relacion11_3 = (usuario11, usuario3)
relacion11_4 = (usuario11, usuario4)
relacion11_5 = (usuario11, usuario5)
relacion11_6 = (usuario11, usuario6)
relacion11_7 = (usuario11, usuario7)
relacion11_8 = (usuario11, usuario8)
relacion11_9 = (usuario11, usuario9)
relacion11_10 = (usuario11, usuario10)
relacion11_12 = (usuario11, usuario12)

publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario5])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])
publicacion1_5 = (usuario1, "Este es como mi quinto post", [usuario5])
publicacion1_6 = (usuario1, "Esta me la likeo yo", [usuario1])

publicacion2_1 = (usuario2, "Hello World", [usuario4])
publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario4])

publicacion3_1 = (usuario3, "Lorem Ipsum", [])
publicacion3_2 = (usuario3, "dolor sit amet", [usuario2])
publicacion3_3 = (usuario3, "consectetur adipiscing elit", [usuario2, usuario5])
publicacion3_4 = (usuario3, "Autolike", [usuario2, usuario3])
publicacion3_5 = (usuario3, "2da publicacion sin likes", [])

publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
publicacion4_2 = (usuario4, "I am Bob", [])
publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])
publicacion4_4 = (usuario4, "estoy contento, mas de un fiel!", [usuario1, usuario2])

publicacion5_1 = (usuario5, "Estoy testeando un programa", [])
publicacion5_2 = (usuario5, "Mi 1er autolike", [usuario5])
publicacion5_3 = (usuario5, "Mi 2do autolike", [usuario5])

publicacion10_1 = (usuario10, "fulbo", [usuario6, usuario7, usuario8])

publicacion11_1 = (usuario11, "Quisiera tener un millon de amigos", [usuario5])
publicacion11_2 = (usuario11, "Soy Roberto", [usuario7, usuario8])

usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)

-- Redes definidas para los test suite

usuariosC = [usuario1, usuario2]
relacionesC = [relacion1_2]
publicacionesC = []
redC = (usuariosC, relacionesC, publicacionesC)

usuariosD = [usuario1, usuario2]
relacionesD = []
publicacionesD = []
redD = (usuariosD, relacionesD, publicacionesD)

usuariosE = [usuario1, usuario2, usuario3]
relacionesE = []
publicacionesE = [publicacion3_4]
redE = (usuariosE, relacionesE, publicacionesE)

usuariosF = [usuario1, usuario2, usuario3, usuario4, usuario5]
relacionesF = [relacion2_4, relacion1_4]
publicacionesF = [publicacion1_6, publicacion2_1, publicacion3_1, publicacion3_5, publicacion5_2, publicacion5_3, publicacion4_1, publicacion4_4]
redF = (usuariosF, relacionesF, publicacionesF)

usuariosR = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11]
relacionesR = [relacion11_1, relacion11_2, relacion11_3, relacion11_4, relacion11_5, relacion11_6, relacion11_7, relacion11_8, relacion11_9,
               relacion11_10, relacion11_12, relacion6_10, relacion7_10, relacion8_10]
publicacionesR = [publicacion5_1, publicacion10_1, publicacion11_1, publicacion11_2]
redRoberto = (usuariosR, relacionesR, publicacionesR)

redCasiRoberto = (usuariosR, relacionesB, []) 

usuariosU = [usuario1]
relacionesU = []
publicacionesU = []
redUnica = (usuariosU, relacionesU, publicacionesU)

redVacia = ([], [], [])

